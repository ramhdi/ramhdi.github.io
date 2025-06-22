---
title: "Developing a REST API Monitor: Lessons Learned"
---

## Background

At work, my team was tasked with integrating real-time IoT data into a dashboard for monitoring and storage. The requirements seemed straightforward: request data via REST API, perform some processing, then send it to both our dashboard and database. How hard could it be? Just configure a REST API client to make periodic requests and forward the results to consumers.

Unfortunately, the task wasn\'t that straightforward.

What we discovered was that API downtimes were far more common than anticipated. In addition to simple connectivity issues, we encountered another significant challenge: stale data. Since our use case demanded real-time information, receiving outdated responses was almost as problematic as receiving no response at all. This led us down a path of building customized REST API monitoring solution.

Our objectives materialized into two key metrics: measuring both the uptime and the up-to-date percentage of our REST API endpoints. These metrics became crucial not only for our internal team\'s performance evaluation but also for meeting the reliability standards our client demanded.

## First Iteration: Uptime Kuma

Our initial approach was to leverage existing tools, and [Uptime Kuma](https://github.com/louislam/uptime-kuma) emerged as the natural first choice. The open-source monitoring solution offered advantages:

- Completely open source with an active community
- Straightforward configuration with minimal setup overhead
- Rich and informative interface including historical charts for key metrics

During our initial testing phase, everything appeared to work seamlessly. However, fundamental limitations became apparent. Our API requests required dynamic query parameters—specifically, start and end timestamps that needed to be calculated at runtime. Uptime Kuma\'s static configuration model simply could not accommodate this requirement.

More critically, Uptime Kuma lacked the ability to programatically parse response content. This meant we had no way to detect stale data scenarios where the API returned a successful HTTP status but with outdated or empty payloads. For a real-time monitoring system, this was a deal-breaker.

The realization was clear: we needed a custom solution tailored to our specific requirements.

## Second Iteration: Building with Rust

For our custom REST API monitor, we chose Rust as our foundation language. The decision was driven by Rust\'s exceptional performance coupled with its modest resource consumption—a critical consideration since our \'simple\' monitoring tool should not become a resource burden on our infrastructures.

Our technology stack evolved around proven libraries:

- **[Tokio](https://tokio.rs/)** for asynchronous runtime management
- **[Axum](https://github.com/tokio-rs/axum)** for our HTTP server implementation
- **[Reqwest](https://github.com/seanmonstar/reqwest)** for HTTP client functionality
- **[SQLite](https://sqlite.org/)** with **[SQLx](https://github.com/launchbadge/sqlx)** for data persistence (chosen for rapid development)

The initial architecture followed a straightforward approach:

```rust
async fn monitor_loop(state: AppState, assets: &Vec<Asset>) {
    let mut api1_interval = tokio::time::interval(StdDuration::from_secs(10));
    let mut api2_interval = tokio::time::interval(StdDuration::from_secs(300));
    let mut global_api_interval = tokio::time::interval(StdDuration::from_secs(10));

    loop {
        tokio::select! {
            _ = api1_interval.tick() => {
                for asset in assets {
                    let (status, response_time, error) = check_api1(&state.client, &asset.device_id, &auth_token).await;
                    store_check_result(&state.db, &asset.id, "Api1", status, response_time, error).await;
                }
            }

            _ = api2_interval.tick() => {
                for asset in assets {
                    let apis = vec![
                        ("Api2a", "https://api.example.com/endpoint1"),
                        ("Api2b", "https://api.example.com/endpoint2"),
                        ("Api2c", "https://api.example.com/endpoint3"),
                    ];

                    for (api_type, endpoint) in apis {
                        let (status, response_time, error) = check_api2(&state.client, endpoint, &asset.device_id).await;
                        store_check_result(&state.db, &asset.id, api_type, status, response_time, error).await;
                    }
                }
            }

            _ = global_api_interval.tick() => {
                let (status, response_time, error) = check_global_api(&state.client).await;
                store_check_result(&state.db, None, "GlobalApi", status, response_time, error).await;
            }
        }
    }
}
```

We deployed the solution and began collecting production data. Within 24 hours, however, serious issues became apparent.

The next morning when we checked our monitoring dashboard, database queries were noticeably slow, and further checks revealed our SQLite file had grown to 60 MB in just one day. At this rate, we would consume several gigabytes within a month and potentially tens of gigabytes over a year.

Code analysis revealed an additional architectural flaw: our monitors operated within a shared execution context, meaning a slow or hanging request from one monitor could block others from executing. This meant that the monitors were not isolated from each other, and therefore not optimal.

## Third Iteration: Architectural Optimization

The problems we encountered demanded a fundamental redesign rather than incremental fixes. Our optimization strategy focused on three core improvements:

### 1. Status-Change-Only Storage

Instead of storing every monitoring check result, we implemented a system that only persisted actual status changes.

```rust
pub async fn check_and_update_monitor_state(
    monitor_id: &str,
    asset_id: Option<&str>,
    api_type: &ApiType,
    new_status: CheckStatus,
    response_time_ms: Option<i64>,
    error_message: Option<String>,
    states: &MonitorStates,
    db: &sqlx::PgPool,
) -> anyhow::Result<()> {
    let mut states_guard = states.write().await;
    let current_state = states_guard.get(monitor_id);

    let is_first_check = current_state.map(|s| !s.has_been_checked).unwrap_or(true);
    let status_changed = if is_first_check {
        true
    } else {
        current_state.unwrap().current_status != new_status
    };

    if status_changed {
        // Store status change to database
        store_status_change(
            db,
            monitor_id,
            asset_id,
            api_type,
            old_status,
            new_status,
            response_time_ms,
            error_message,
        ).await?;
    }

    // Always update in-memory state
    states_guard.insert(
        monitor_id.to_string(),
        MonitorState {
            current_status: new_status,
            last_change_time: if status_changed { Utc::now() } else { previous_change_time },
            last_check_time: Some(Utc::now()),
            latest_response_time: response_time_ms,
            latest_error: error_message,
            has_been_checked: true,
        },
    );

    Ok(())
}
```

This approach should reduce our storage requirements by a scale linear to the uptime percentage.

### 2. In-Memory State Management

We implemented a concurrent hash map to maintain the current state of all monitors in memory:

```rust
use std::collections::HashMap;
use tokio::sync::RwLock;

type MonitorStates = Arc<RwLock<HashMap<String, MonitorState>>>;

struct MonitorState {
    current_status: Status,
    last_change_time: DateTime<Utc>,
    latest_response_time: Option<Duration>,
    error_message: Option<String>,
}
```

This eliminated the need for frequent database queries to determine current status and enabled quick status lookups. As the number of monitors is fixed, the hash map is expected not to grow without bounds.

### 3. Monitor Isolation Through Task Spawning

Each monitor now runs as an independent Tokio task, preventing any slow API call from affecting others:

```rust
async fn spawn_monitor_task(&self, monitor: Monitor) -> MonitorTask {
    let monitor_id = monitor.id.clone();
    let deps = self.deps.clone();
    let mut shutdown_rx = self.shutdown_tx.subscribe();

    let handle = tokio::spawn(async move {
        let mut interval = tokio::time::interval(
            StdDuration::from_secs(monitor.interval_seconds as u64)
        );

        // Add random jitter to avoid making requests on the same time
        let jitter = fastrand::u64(0..5000);
        tokio::time::sleep(StdDuration::from_millis(jitter)).await;

        loop {
            tokio::select! {
                _ = interval.tick() => {
                    if let Err(e) = execute_monitor_check(&monitor, &deps).await {
                        error!("Monitor {} check failed: {}", monitor.id, e);
                    }
                }
                _ = shutdown_rx.recv() => {
                    info!("Monitor task {} received shutdown signal", monitor.id);
                    break;
                }
            }
        }
    });

    MonitorTask { monitor_id, handle }
}
```

### 4. Database Migration to TimescaleDB

To future-proof our solution, we migrated from SQLite to TimescaleDB, a Postgres-based time-series database. This change provided optimized storage and query performance for time-series data. In addition, we are already familiar with Postgres database, and more importantly easier debugging compared to SQLite.

```sql
-- TimescaleDB hypertable creation
CREATE TABLE status_changes (
    id UUID PRIMARY KEY,
    monitor_id TEXT NOT NULL,
    old_status TEXT,
    new_status TEXT NOT NULL,
    changed_at TIMESTAMPTZ NOT NULL,
    response_time_ms INT8,
    error_message TEXT
);

SELECT create_hypertable(\'status_changes\', \'changed_at\');
```

## Performance Impact and Results

The optimizations delivered improvements across various metrics:

**Storage Efficiency:**

- Number of records: from ~150,000 monitor entries reduced to ~50 status change entries daily
- Database file growth stabilized at manageable levels

**Query Performance:**

- Database queries went from multi-second responses to sub-100ms
- Dashboard load times improved from 5+ seconds to under 1 second

## Lessons Learned: Technical and Architectural Insights

This project unexpectedly helped us learn valuable lessons:

### Concurrency Patterns in Rust

Working with Tokio introduced us to powerful concurrency patterns:

- **Select and join macro** for handling multiple async operations simultaneously
- **Task spawning** for executing concurrent and independent tasks
- **Channels** for safe inter-task communication and especially for sending shutdown signals
- **Arc and RwLock** for thread-safe state management across async boundaries

### Database Design Considerations

The project showed us database optimization principles:

- **Time-series data** requires specialized storage strategies
- **Storage growth projections** should be calculated before, not after, deployment
- **Database choice** significantly impacts development experience, performance and operational complexity

### System Architecture Insights

The evolution from a simple monitoring loop to independent task-based system demonstrated:

- **Isolation principles** prevent cascading failures in distributed systems
- **State management strategies** must be carefully implemented especially in concurrent setting
- **Storage patterns** should be chosen based on access patterns and the nature of the data itself, not just convenience

## Looking Forward

The final system now reliably monitors multiple endpoints across different time intervals, provides accurate uptime and up-to-date metrics, and maintains reasonable resource utilization. More importantly, it serves as a foundation for future monitoring requirements and has become a valuable reference implementation for similar projects within our team.
