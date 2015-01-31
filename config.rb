set :enviroment, :development

activate :livereload
activate :syntax

set :title, 'lpil'

set :css_dir, 'css'
set :js_dir, 'js'
set :images_dir, 'images'

set :markdown_engine, :redcarpet
set :markdown, fenced_code_blocks: true,
               smartypants: true

configure :build do
  activate :minify_css
  activate :minify_javascript
  activate :relative_assets
  set :enviroment, :production
end

activate :blog do |blog|
  blog.layout = 'blog'
  blog.permalink = 'blog/:title'
  blog.sources = 'posts/:year-:month-:day-:title.html'
end
activate :directory_indexes # Must go after blog

page 'feed.xml', layout: false
