---
title: An Exercism Syllabus for Gleam
hidden: true
tags:
  - Gleam
  - Exercism
---

For the last few months I've been working with the Erlang Ecosystem Foundation and the Exercism team to create a Gleam syllabus for the Exercism platform, and as of this morning the final lesson has been merged!

## What does that mean?

Just to make sure we'll all on the same page: [Gleam][gleam] is a type safe functional programming language that runs on the Erlang virtual machine alongside languages like Erlang, Elixir, and LFE. It was created by me, Louis Pilfold, and is now maintained by the Gleam core team.

[The Erlang Ecosystem Foundation][eef] is a non-profit organisation that supports the BEAM ecosystem, including all languages and techonologies built upon the BEAM.

[Exercism][exercism] is a delighful platform for learning and practicing programming languages, incorporating lessons, automated challenges, and the option of feedback from mentors who know the language well. It has tracks for 67 languages, including 4 BEAM languages: Erlang, Elixir, LFE, and Gleam. Notably Exercism is entirely free to use!

Tracks on Exercism comprise a series of Exercises students can use to practice a language, however some tracks also have a syllabus of lessons teach the language. Each language concept is carefully introduced and explained in an order that builds upon previous lessons, and each lesson has a set of exercises to practice the concept, taking the student from beginner to proficient and ready to solve real world problems.

[eef]: https://erlef.org/
[exercism]: https://exercism.org/
[gleam]: https://gleam.run/

<img src="/img/blog/gleam-exercism-syllabus/tree.png" alt="TODO!!!">

## What did we build?

Elixir's syllabus is largely regarded as the gold standard for Exercism tracks, and it is the yard-stick we used when building the Gleam syllabus. The goal was to create a syllabus that would be as good as Elixir's, and I think we've done it.

Before we could start working on the educational content we had to create a [custom Gleam test runner][test-runner] which implemented version 2 of the Exercism test runner interface. The creation of this runner also produced a set of libraries or significant improvements to existing Gleam libraries, including:

- [Glexer, a Gleam tokenizer](https://github.com/DanielleMaywood/glexer/)
- [Glance, a Gleam parser](https://github.com/lpil/glance/)
- [Gap, a diffing library](https://github.com/JohnBjrk/gap)
- [Simplifile, a file system library](https://github.com/bcpeinhardt/simplifile)
- [gleam_erlang, bindings to standard Erlang APIs](https://github.com/gleam-lang/erlang)

Work has also begun two other static anaylsis tools for Gleam, which will be later incorporated into Exercism to provide futher automated feedback. They would also make a strong foundation for a generic Gleam linter that can be used by software development teams using Gleam.

[test-runner]: https://github.com/exercism/gleam-test-runner

Afterwards the language was divided into a set of concepts, and the concepts arranged into dependency order such that each concept builds upon the previous. The existing exercises were then assigned to the concepts, depending on what language features the student would likely need to know to solve the exercise.

As each concept lesson was written a corresponding exercise would be created alongside it. This exercise would be especially focused on the concept being taught, and is intended to solidify the student's understanding, while not being too time consuming to solve.

The initial set of 12 concepts and 12 accompanying exercises were launched on the 4th of July, alongside the existing 92 exercises. Further concepts and exercises were added over the following months with the final being merged this morning on the 28th of September, bringing the track up to [36 concepts][concepts] and [122 exercises][exercises]. Gleam is now one of the most comprehensive tracks on Exercism, standing alongside Elixir!

Throughout the development period I and the other Gleam mentors have been offering feedback to students on their exercise submissions, and we will continue to do so into the future.

[exercises]: https://exercism.org/tracks/gleam/exercises
[concepts]: https://exercism.org/tracks/gleam/concepts

## How was it received?

I am rather fond of keeping some stats, so I wrote a small Gleam script that scrapes the Exercism website once a day and records how much activity there was on the track for that day. Here's a couple of charts:

### Number of students

<img src="/img/blog/gleam-exercism-syllabus/students.svg" alt="TODO!!!">

When the first concepts were launched the track had 474 students, and today there are 627 students. The rate at which students joined the course increased after the first concepts were launched in late July.

### Number of exercise submissions

<img src="/img/blog/gleam-exercism-syllabus/submissions.svg" alt="TODO!!!">

The impact of the concepts exercise is much more striking when looking at the number of exercise submissions by Gleam students, the rate greatly increasing after the first concepts were launched. Students are much more engaged with the material than before, spending more time with the track. Since the first concepts were launched there have been 6,936 exercise submissions.

### Testimonials

As well as quantitative data I have asked students for feedback on the track, here are some of their responses:

> I'm working through [the syllabus] at the moment and I've personally found them incredibly helpful for helping me focus on solidifying my understanding of specific concepts. The use of multiple examples for each concept helps round out my understanding of how things work a lot better than just reading docs, and having guided exercises that are focused purely on learning a concept at a time helps me gain traction a lot better than coding blindly

And here are some comments from students on their mentoring sessions:

> Thanks for being really friendly and helpful, and taking the time to write out some really great explanation.

> Thanks gain Louis, Gleam has excellent documentation, an excellent exercism course and... a special mentor!

> Thank you for taking time too help out. It was really helpfull and a good explenation as to why a particular opperator worked as it did.

> This was my first mentoring session on Exercism. It will have a special place in my heart having had the creator stopping by and leave a nice word! Thanks 💜

> He helped me to understand the language's idiomatic expressions look like, and recommended ways to rethink the problem to fit more naturally in Gleam.

> Yeah... thanks a lot.. I'll be applying my learnings to the other exercises. I think it was good for me to study a bigger code example to get a feel for gleam !

> He pointed out where I could improve the solution with built-in functions that expressed the code's intent perfectly. This is helping me learn Gleam's toolkit.

## Thanks!

I would like to thank the Erlang Ecosystem Foundation for supporting me in this work, the Exercism team for their guidance and help, and all [the contributors][contributors] who helped build the Gleam track. And extra special thank you goes to [Jeremie Gillet][jiegillet], [Erik Schierboom][ErikSchierboom], and [Giacomo Cavalieri][giacomocavalieri] for their outstanding contributions.

[contributors]: https://exercism.org/contributing/contributors?track_slug=gleam&page=1
[jiegillet]: https://exercism.org/profiles/jiegillet
[giacomocavalieri]: https://github.com/giacomocavalieri
[ErikSchierboom]: https://exercism.org/profiles/ErikSchierboom

If you appreciate Gleam or Exercism please consider supporting the projects. Both are open source, entirely free to use, and rely on community sponsorship to continue development. Thank you.

- [Exercism's donation page](https://exercism.org/donate)
- [Exercism on GitHub sponsors](https://github.com/sponsors/exercism)
- [Gleam on GitHub sponsors](https://github.com/sponsors/lpil)
