set :enviroment, :development

activate :livereload,
         host: 'localhost',
         apply_js_live: false,
         apply_css_live: false,
         no_swf: true

activate :syntax

set :site_url, 'http://lpil.uk'
set :title, 'lpil'
set :description, 'The website of a code monkey named Louis Pilfold.'

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
