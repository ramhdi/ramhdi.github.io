--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.List (sortBy)
import Data.Monoid (mappend)
import Data.Ord (comparing)
import Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "assets/*" $ do
    route idRoute
    compile copyFileCompiler

  match "projects/*/assets/*" $ do
    route idRoute
    compile copyFileCompiler

  match "resume/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match "js/*" $ do
    route idRoute
    compile copyFileCompiler

  -- Blog posts
  match "posts/*" $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

  -- Projects
  match "projects/*/index.md" $ do
    route $ gsubRoute "/index.md" (const ".html")
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/project.html" projectCtx
        >>= loadAndApplyTemplate "templates/default.html" projectCtx
        >>= relativizeUrls

  -- Blog index
  create ["blog.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let blogCtx =
            listField "posts" postCtx (return posts)
              `mappend` constField "title" "Blog"
              `mappend` defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/blog.html" blogCtx
        >>= loadAndApplyTemplate "templates/default.html" blogCtx
        >>= relativizeUrls

  -- Projects index
  create ["projects.html"] $ do
    route idRoute
    compile $ do
      projects <- recentFirst =<< loadAll "projects/*/index.md"
      let projectsCtx =
            listField "projects" projectCtx (return projects)
              `mappend` constField "title" "Projects"
              `mappend` defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/projects.html" projectsCtx
        >>= loadAndApplyTemplate "templates/default.html" projectsCtx
        >>= relativizeUrls

  -- Home page
  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      projects <- recentFirst =<< loadAll "projects/*/index.md"
      let indexCtx =
            listField "posts" postCtx (return (take 3 posts))
              `mappend` listField "projects" projectCtx (return (take 3 projects))
              `mappend` defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    `mappend` dateField "isoDate" "%Y-%m-%d"
    `mappend` defaultContext

projectCtx :: Context String
projectCtx =
  dateField "date" "%B %e, %Y"
    `mappend` dateField "isoDate" "%Y-%m-%d"
    `mappend` defaultContext