---
title:  "i3wm: Opening applications in the scratchpad"
categories:
  - i3wm
---

I'm a *huge* fan of the [i3 window manager][i3wm]. It's minimalistic, it tiles,
it's modal, it's dynamic, what's not to like?

While fiddling with my config tonight I've stumbled across a little trick you
may find useful: opening applications in the scratchpad with a keybind. With
this configuration I can hit `alt`+`m`, and bam, ncmpcpp, my music browser
application, pops up over my work in the floating scratchpad window. And
because it's in the floating scratchpad, it doesn't disturb my window layout.
Super handy.

## Configuration

First off we're going to need to tell i3 what to do with the applications we
wish to be in the scratchpad. Here I'm going to mark these applications with
the instance value of "`scratchpad`", but you can use whatever you want to.

    for_window [instance="scratchpad"] move window to scratchpad, scratchpad show

With this line in our `~/.i3/config` file i3 now knows to move any window with
this instance value to the scratchpad, and then display the scratchpad. Now,
for actually launching the applications...

    bindsym Mod1+m exec --no-startup-id urxvt -name scratchpad -e ncmpcpp

Lets break this down.

* `bindsym Mod1+m` is registering an action to the key combination of Mod1
  (alt), and the `m` key
* `exec` causes the following shell code to be executed when the keys are
  pressed
* `--no-startup-id` disables the startup-notification feature, as the terminal
  I'm using, urxvt, does not support this
* `urxvt` is the terminal emulator I'm using
* `-name scratchpad` causes the terminal window to have the instance of
  "scratchpad", which is what we specified behaviour for earlier. I expect
  there is a similar flag available for other terminal emulators
* `-e ncmpcpp` causes the application I desire to run in the spawned terminal

And there we have it! If we change the key combination and swapping `ncmpcpp`
for another console application, we'll have that application open in the same
fashion, at the touch of a button. You could have one button for music, one for
your email client, another to reattach to a tmux or screen session containing
your IRC client. Any application where you would want to quickly summon it up
and give it your full attention for a few seconds this would be ideal for.

Hope you've found this useful. If you've got any other tricks to share, shoot
me an email :)

Cheers,
Louis

[i3wm]: http://i3wm.org/
