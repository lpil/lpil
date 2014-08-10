---
---
# Email addresses in CSS have been muddled for anti-spam reasons. Fix them.
unmuddle_emails = ->
  muddled_emails = document.getElementsByClassName 'js-muddled-email'
  unmuddle = (email) ->
    email.href = email.href.replace /my-first-name/, 'louis'
    email.href = email.href.replace /this-domain/, 'lpil.uk'
  unmuddle muddled_email for muddled_email in muddled_emails

# Add CSS link for 'Awesome Font' icons
add_aw_font_link = ->
  aw_font_link = document.querySelector '.js-awesome-font-link'
  aw_font_link.href =
    '//maxcdn.bootstrapcdn.com/font-awesome/4.1.0/css/font-awesome.min.css'

done_already = false
# Run them when the page is interactive
document.onreadystatechange = ->
  document_readyState =  document.readyState
  if document_readyState == 'interactive' or 'complete'
    return if done_already
    unmuddle_emails()
    add_aw_font_link()