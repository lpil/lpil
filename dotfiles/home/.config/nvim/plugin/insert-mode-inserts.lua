-- Create a mapping under <C-r> that inserts some given text
local function insert(keys, desc, fn)
  local chord = '<C-r>' .. keys
  vim.keymap.set('i', chord, function()
    vim.api.nvim_put({fn()}, 'c', true, true)
  end, { desc = desc })
end

insert('d', 'Insert current date', function()
  return os.date('%Y-%m-%d')
end)

insert('t', 'Insert current time', function()
  return os.date('%H:%M')
end)

insert('s', 'Insert timestamp', function()
  return os.date('%Y-%m-%d %H:%M')
end)

insert('p', 'Insert file path', function()
  return vim.fn.expand('%:p')
end)

insert('f', 'Insert filename', function()
  return vim.fn.expand('%:t')
end)

insert('u', 'Insert UUID', function()
  return vim.fn.system('uuidgen'):gsub('%s+', ''):lower()
end)

-- Get the current version from ./gleam.toml and insert a new changelog entry
-- header for that version and the current date.
insert('cl', 'Insert changelog release heading', function ()
  local f = io.open('./gleam.toml', 'r')
  if not f then return '' end
  local contents = f:read('*a')
  f:close()
  local version = contents:match('version = "([^"]+)"')
  if not version then return '' end
  return '## v' .. version .. ' - ' .. os.date('%Y-%m-%d')
end)
