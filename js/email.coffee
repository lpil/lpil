---
---
emails = document.getElementsByClassName 'js-muddled-email'
unmuddle = (email) ->
  email.href = email.href.replace /my-first-name/, 'louis'
  email.href = email.href.replace /this-domain/, 'lpil.uk'
unmuddle email for email in emails
