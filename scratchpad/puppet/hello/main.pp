#
# I want a configuration management tool that will clean up resources after
# that have been removed from the code, like Terraform does. It seems that
# Puppet is not that tool. :(
#

class helloworld {
  notify { 'hello, world!': }
}

node default {
  class { 'helloworld': }
  # class { 'helloworld::motd': }
}

# class helloworld::motd {
#   file { '/tmp/hello':
#     content => "hello, world!\n",
#   }
# }
