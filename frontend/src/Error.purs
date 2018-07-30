module Error where

import LocalCooking.Common.Blog (BlogPostVariant)

import Prelude
import Data.String.Permalink (Permalink)


data RedirectError
  = RedirectNewBlogPostNoEditor


data SiteError
  = RedirectError RedirectError
  | SiteErrorBlogPostCategoryNotFound BlogPostVariant Permalink
  | SiteErrorBlogPostCategoryNotUnique BlogPostVariant Permalink
  | SiteErrorBlogPostNotFound BlogPostVariant Permalink Permalink
  | SiteErrorBlogPostNotUnique BlogPostVariant Permalink Permalink
  | SiteErrorEditorDoesntExist

printSiteError :: SiteError -> String
printSiteError e = case e of
  RedirectError r -> case r of
    RedirectNewBlogPostNoEditor -> "Redirected - Can't create new blog post without credentials!"
  SiteErrorBlogPostCategoryNotFound variant cat -> "Internal Error - Can't find blog post category: " <> show variant <> ", " <> show cat
  SiteErrorBlogPostCategoryNotUnique variant cat -> "Internal Error - Blog post category isn't unique: " <> show variant <> ", " <> show cat
  SiteErrorBlogPostNotFound variant cat post -> "Internal Error - Can't find blog post: " <> show variant <> ", " <> show cat <> ", " <> show post
  SiteErrorBlogPostNotUnique variant cat post -> "Internal Error - Blog post isn't unique: " <> show variant <> ", " <> show cat <> ", " <> show post
  SiteErrorEditorDoesntExist -> "Internal Error - Editor doesn't exist"
