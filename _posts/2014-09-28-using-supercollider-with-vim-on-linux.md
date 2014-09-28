---
layout: post
title: Using Supercollider with Vim on Linux
categories:
  - Linux
  - JACK
  - Supercollider
---

[Supercollider][supercollider] is an open source programming language used for
real time audio synthesis and music making. It's really cool. When I first
looked into it I found it a little confusing to set up on Linux as it requires
you use the [JACK][jack] sound server, so I'm going to outline how I set it up
to work with [Vim][vim], my favourite editor.

#### 1: Install Supercollider and JACK

Supercollider and JACK are mature and well established projects, so they're
most likely in your distribution's package manager.

    sudo apt-get install supercollider jackd2

#### 2. Configure JACK

Rather than manually starting JACK each time we want to use Supercollider we
can configure JACK to automatically start when required. First we need to find
out the name of our audio interface.

    louis ~ $ cat /proc/asound/cards
    0 [PCH            ]: HDA-Intel - HDA Intel PCH
                          HDA Intel PCH at 0xf6110000 irq 44
    1 [NVidia         ]: HDA-Intel - HDA NVidia
                          HDA NVidia at 0xf6080000 irq 17
    2 [UDJ6           ]: USB-Audio - UDJ6
                      ESI Audiotechnik GmbH UDJ6...

Here we can see the name of my ESI UDJ6 USB audio interface is `UDJ6`. We can
now write the command that will launch the JACK server. Here's mine:

    /usr/bin/jackd -R -P 95 -d alsa -d hw:UDJ6 -r 44100 -p 256

Once you've written your command, and ensured that it works save it in
`~/.jackdrc`- this will result in JACK automatically starting when required.

Check out the `jackd` man page for more info on the flags used in the command
above.

#### 3. Configure Vim

Install [Stephen 'sbl' Lumenta's][sbl] version of the [scvim][scvim] plugin. I
like to use [gmarik's][gmarik] plugin manager [Vundle][vundle] for this.

With the plugin installed, open a `.sc` file and run the `:SClangStart`
command to start Supercollider in a new terminal emulator window. scvim
[now][scvim-commit] has a sensible default terminal on Linux, so if you're
lucky this will just work. If it doesn't you probably need to set the
`g:sclangTerm` value in your `.vimrc`. Here's an example for `gnome-terminal`:

{% highlight vim %}
    let g:sclangTerm = "gnome-terminal -x $SHELL -ic"
{% endhighlight %}

#### 4. Profit

That's it for setup. Lets see if it works.

Open a new `.sc` file in Vim, run `:SClangStart`, and insert the following
Supercollider code into vim.

{% highlight scala %}
s.boot;
(
  Pbind(
    \freq, Pn(Pseries(110, 111, 10), 2),
    \dur, 1/2,
    \legato, Pwhite(0.1, 1)
  ).play;
    Pbind(
    \freq, Pn(Pseries(220, 222, 10), 4),
    \dur, 1/4,
    \legato, Pwhite(0.1, 1)
  ).play;
  Pbind(
    \freq, Pn(Pseries(330, 333, 10), 6),
    \dur, 1/6,
    \legato, 0.1
  ).play;
)
{% endhighlight %}

Place your cursor on the `s.boot;` line, and hit `F6` to evaluate that line, and
start the Supercollider server. Then place your cursor within the `( )` block
and press `F5` to evaluate that block. Hear some jangly piano noises? Then
you're ready to start making music with Supercollider and Vim :)

---

Special thanks to Carlo Capocasa for his super useful [blog post][carlo-capocasa] on JACK.

[supercollider]: http://supercollider.sourceforge.net/
[jack]: http://www.jackaudio.org/
[vim]: https://en.wikipedia.org/wiki/Vim_(text_editor)
[sbl]: https://github.com/sbl
[scvim]: https://github.com/sbl/scvim
[scvim-commit]: https://github.com/sbl/scvim/commit/9bb1bb89b2f45d79a1c2278b09fc47e5443097ad
[carlo-capocasa]: http://carlocapocasa.com/supercollider-jack-the-easy-way/
[gmarik]: https://github.com/gmarik
[vundle]: https://github.com/gmarik/Vundle.vim
