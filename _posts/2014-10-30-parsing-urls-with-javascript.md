---
layout: post
title: Parsing URLs with Javascript
categories:
  - Javascript
  - Web dev
---

So this is pretty cool:

{% highlight javascript %}
var a = document.createElement('a');
a.href = 'http://louis:passwd@lpil.uk:1991/cool-app/?callback=parseRep#hash';

a.protocol; // => 'http:'
a.username; // => 'louis'
a.password; // => 'passwd'
a.hostname; // => 'lpil.uk'
a.port;     // => '1991'
a.host;     // => 'lpil.uk:1991'
a.pathname; // => '/cool-app/'
a.search;   // => '?callback=parseRep'
a.hash;     // => '#hash'
{% endhighlight %}

The only gotcha seems to be that IE drops the preceding `/` from `pathname`.

Thanks [hasenj](http://stackoverflow.com/questions/8498592/extract-root-domain-name-from-string)
and [jlong](https://gist.github.com/jlong/2428561)! :)
