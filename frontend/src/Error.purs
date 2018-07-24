module Error where


data RedirectError
  = RedirectNewBlogPostNoEditor


data SiteError
  = RedirectError RedirectError

printSiteError :: SiteError -> String
printSiteError e = case e of
  RedirectError r -> case r of
    RedirectNewBlogPostNoEditor -> "Redirected - Can't create new blog post without credentials!"
