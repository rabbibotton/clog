# Common Lisp - The Tutorial Part 1

## The Journey - Introduction

> "If CL is ugly, why do I use it and write about it?
Because Lisp is so powerful that even an ugly Lisp is preferable to
using some other language." - Apr 30, 2002 - C.L.L - Paul Graham

<a
href="https://www.google.com/url?q=https://github.com/rabbibotton/clog/blob/main/LEARN.md&amp;sa=D&amp;source=editors&amp;ust=1719933327555341&amp;usg=AOvVaw2S5PLt8szjqVlb3tSDpa-H"
class="c13">See more at
https://github.com/rabbibotton/clog/blob/main/LEARN.md</a>

### Introduction

Today we are going to embark on a journey together. I, a person with
poor skills at
writing<sup><a href="#ftnt1" id="ftnt_ref1">[1]</a></sup> and the author
of CLOG, the most Awesome GUI and Web Framework on the planet as of
2022<sup><a href="#ftnt2" id="ftnt_ref2">[2]</a></sup>, and you, an
inquisitive individual looking to learn Common Lisp, CLOG and/or
kabbalistic software
design<sup><a href="#ftnt3" id="ftnt_ref3">[3]</a></sup>. The goal of
this journey is to quickly get you up to speed with enough Common Lisp
to use this grotesquely beautiful
language<sup><a href="#ftnt4" id="ftnt_ref4">[4]</a></sup> with millions
of parentheses<sup><a href="#ftnt5" id="ftnt_ref5">[5]</a></sup> to
write awesome software (with CLOG I hope), faster and more powerful than
with any other
language<sup><a href="#ftnt6" id="ftnt_ref6">[6]</a></sup>.

### Rules of the Journey

#### 1. Ignore the parentheses and see only the
    indentations.

The heart of Lisp is the S-expressions (aka the
sexp<sup><a href="#ftnt7" id="ftnt_ref7">[7]</a></sup>).
A parenthesis followed by aמ operator followed by arguments and closed
with another parenthesis. So (+ 1 2) results in a 3 and in almost every
other language is 1 + 2. Data is also expressed in the same way e.g.
(list 1 2 3) the list containing four elements list 1 2 and 3, in memory
it is stored as 1 2 and 3. The expressions nest (list 1 2 (list 2 4
(list 1 3) 4) 3) etc. This means lots and lots of parenthesis.

However, the human brain can parse them with the help of white
space<sup><a href="#ftnt8" id="ftnt_ref8">[8]</a></sup>:

```lisp
(tagbody
  10 (print "Hello")
  20 (go 10))
```

In fact once you start looking at the indentation and
forgetting about the parentheses Lisp starts to look like most
languages.

```lisp
(defun factorial (x)
  (if (zerop x)
    1
    (\* x (factorial (- x 1)))))
```

This is an actual C program from the 1st International
Obfuscated C Code Contest (1984)

```c
a[900]; b;c;d=1 ;e=1;f; g;h;O; main(k,
l)char* *l;{g= atoi(* ++l); for(k=
0;k*k< g;b=k ++>>1) ;for(h= 0;h*h<=
g;++h); --h;c=( (h+=g>h *(h+1)) -1)>>1;
while(d <=g){ ++O;for (f=0;f< O&&d<=g
;++f)a[ b<<5|c] =d++,b+= e;for( f=0;f<O
&&d<=g; ++f)a[b <<5|c]= d++,c+= e;e= -e
;}for(c =0;c<h; ++c){ for(b=0 ;b<k;++
b){if(b <k/2)a[ b<<5|c] ^=a[(k -(b+1))
<<5|c]^= a[b<<5 |c]^=a[ (k-(b+1 ))<<5|c]
;printf( a[b<<5|c ]?"%-4d" :" " ,a[b<<5
|c]);} putchar( '\n');}} /*Mike Laman*/
```

The simple regular syntax of Lisp and its use of
parentheses is part of what makes Lisp so powerful, it is called
homoiconicity. For now take my word on this, but it is omnipotent power
made touchable by parentheses.

There is no place for discrimination! Do not judge a
language based on its parentheses!

#### 2. Clear your mind, use the force luke! (Star Wars)

Lisp is a multi paradigm language. If it is a buzzword, Lisp invented
it, has it (and did it better), or a few lines of code and will have it
(it is the programmable language by
design)<sup><a href="#ftnt9" id="ftnt_ref9">[9]</a></sup>. It is not a
mistake to write code using any paradigm as long as it is crisp and
clean and no
caffeine<sup><a href="#ftnt10" id="ftnt_ref10">[10]</a></sup>, i.e.
readable and fitting the
domain<sup><a href="#ftnt11" id="ftnt_ref11">[11]</a></sup>, error free
and gets the job
done<sup><a href="#ftnt12" id="ftnt_ref12">[12]</a></sup>.

  - Functional - You bet, Lisp and Alonzo Church gave meaning to the
    letter lambda<sup><a href="#ftnt13" id="ftnt_ref13">[13]</a></sup>.
  - Object Oriented - Your orientation is accepted here,
    we are all CLOS
  - Procedural - It is our dirty little secret
  - Structural - That goto example will haunt me
  - Etc etc

#### 3. Stability Matters

Lisp was specified in 1958, its first full compiler in 1962, and evolved
into Common Lisp which became ANSI standard in 1994 and has been stable
ever since<sup><a href="#ftnt14" id="ftnt_ref14">[14]</a></sup>. The
real secret of
success<sup><a href="#ftnt15" id="ftnt_ref15">[15]</a></sup> is building
a foundation, perfecting it, only when absolutely necessary rewriting
it<sup><a href="#ftnt16" id="ftnt_ref16">[16]</a></sup>.

The Ada version of CLOG has been around since 2013,
CLOG the Common Lisp version is incrementally the same design with much
more built on top. Some libraries CLOG sits on are older than most of
you.

When you see a github Common Lisp project with a very
old date, that just means it is stable and deserves a look.

Experience is fine wine, stability is the tortoise that wins over the
hare,
always<sup><a href="#ftnt17" id="ftnt_ref17">[17]</a></sup>.

#### 4. Community Matters

I have found in joining the Lisp community fantastic
people with tons of experience all willing to contribute their
experience and knowledge. They are brutal for their lingo, and
rightfully so, beside the only way to communicate succinctly and make
sure all on the same page, how we react to rebuke quickly shows who and
what we are and if worth spending the time to communicate with.

I frequent \#commonLisp on libera.chat thankfully they keep the channel
strongly on topic and away from the greatest wastes of time on earth.
Governmental politics. I am dbotton there and I have very strong
feelings about never being anonymous online and that keeps me from
saying things I should regret
later<sup><a href="#ftnt18" id="ftnt_ref18">[18]</a></sup>.
There is also Lisp discord and <https://www.cliki.net/> a
wiki that will get you to many more resources.

The Open Source movement and Lisp are connected at the
hip<sup><a href="#ftnt19" id="ftnt_ref19">[19]</a></sup>. My fellow
journeyer I have written insane amounts of free as in freedom lines of
code and there is nothing more satisfying than knowing my code
contributes to the advancement of us
all<sup><a href="#ftnt20" id="ftnt_ref20">[20]</a></sup>. The return
with many years of doing this is making a living and loving life (the
two rarely go together). Vertical
development<sup><a href="#ftnt21" id="ftnt_ref21">[21]</a></sup> is
where there is always money to be made, but horizontal development needs
to be free as in freedom, and sometimes, but not always, that means
doing work for free as in beer.

#### 5. Tools Matter

The success of a language is all about the tools and
their open source status. Common Lisp is the most successful of all time
with a crazy number of open source compilers (most still maintained!)
and commercial compilers each with amazing tooling. Respect your tools.
Assuming your code is the cause of the error not the compiler and your
compiler will always be your friend.

### Where our journey will take us

As I am trench
programmer<sup><a href="#ftnt22" id="ftnt_ref22">[22]</a></sup>,
I am not the one to write the standard, the manual, the
foundation for your development as a programmer, but I can excite you
and get you writing Common Lisp with CLOG and that is my focus
here.

I hope to cover:

- A minimal set of operators to write business/IT
  applications.
- Learn basic Lisp idioms
- Get you motivated to look into the depths of Lisp and
  Software Development in general.
- From early on learn about and how to use parallel computing -
  yes that is a minimum
  today!
- And of course learn to be a
  CLOGer<sup><a href="#ftnt23" id="ftnt_ref23">[23]</a></sup>

---

<a href="#ftnt_ref1" id="ftnt1">[1]</a> If only word
processors had compilers, debuggers and profilers

<a href="#ftnt_ref2" id="ftnt2">[2]</a> I know this
to be true since CLOG is more awesome than GNOGA my Ada version from
2013 and until CLOG came around nothing was as awesome as it. I can also
say this since there are only a few frameworks I know of that are
designed from scratch for both web and gui use. Oh, one last proof, this
fact has a footnote and is now on the internet so must be true.

<a href="#ftnt_ref3" id="ftnt3">[3]</a> Which
doesn't exist but if it did it would be in a Hebrew version of
Lisp.


<a href="#ftnt_ref4" id="ftnt4">[4]</a> Beauty is in
the eye of the beholder, but Jean Ichbiah was an artist. To elaborate
further, the author of Lisp was John McCarthy, a Litvach (a Lithuanian
Ashkenazi Jew) and Jean Ichbiah was the author of Ada (a Turkish
Sefardic Jew). In the world of jews the
litvachs<span class="c5 c6"> are
stereotyped as the scientific types and the sefardic the artistic type,
so you see this is all a very scientific analysis. I am a mixed breed so
I write in Ada and Lisp.

<a href="#ftnt_ref5" id="ftnt5">[5]</a> In 1987, I
wanted to teach myself about AI programming. I was 11 then but I had
already written very cool software in Assembly, Basic, Pascal, and lots
of other languages but wanted more. So I asked Jack, the computer
teacher at Nova University (they still have a k-12 division) who got me
started programming with a bribe for the formula for how to draw circles
if I could prove I would know what to do with it, what the best language
would be, he said "prolog or Lisp?" I took one look at the parentheses
and grabbed the prolog floppy.

<a href="#ftnt_ref6" id="ftnt6">[6]</a> "Lisp is no
harder to understand than other languages. So if you have never learned
to program, and you want to start, start with Lisp." RMS

<a href="#ftnt_ref7" id="ftnt7">[7]</a> Lisp really
does have sexapeal.

<a href="#ftnt_ref8" id="ftnt8">[8]</a> Yes Lisp is
from the 1960's and the classics like  - 10 print "hello" 20 goto 10 was
super cool and Lisp  was served on punch cards too. Lisp, a little known fact,  introduced throughout the history of software engineering most
cool™ concepts we take for granted like if-then-else (as cond),
automatic garbage collection, and was used for the first implementation
of JavaScript (which is very Lispy).

<a href="#ftnt_ref9" id="ftnt9">[9]</a> That is the
case in Common Lisp and most Lisps, but not all Lisps are the same. From here on in, when we say Lisp we mean the ugly duckling that made it through years of college in the AI era, standardized as Common Lisp,
then 30+ years of real world industrial experience still in production
and, because of experimentation with psychedelics, is not all baby skin, but looks awesome even though is the second oldest language in
production use after Fortran.

<a href="#ftnt_ref10" id="ftnt10">[10]</a> 7up, the
source of that catchy phrase, is good stuff and not sticky like Sprite.
Why are fat people discriminated against and gas stations and best buys
only have diet Coke! Only skinny people drink diet cola, we larger folk
want flavor, Diet Dr Pepper, Diet Fanta, etc. that is why we are
fat!

<a href="#ftnt_ref11" id="ftnt11">[11]</a> Define
abstractions relevant to the problem domain and keep re-using
them!

<a href="#ftnt_ref12" id="ftnt12">[12]</a> "I am not
a devotee of functional programming. I see nothing bad about side
effects and I do not make efforts to avoid them unless there is a
practical reason. There is code that is natural to write in a functional
way, and code that is more natural with side effects, and I do not
campaign about the question." RMS

<a href="#ftnt_ref13" id="ftnt13">[13]</a> Some
refer to CL as being only functional, yet on some wiki's they run out of
disk space with those saying it is not a functional language. Just
remember rule \#2 clear your mind.

<a href="#ftnt_ref14" id="ftnt14">[14]</a> Common
Lisp and Dr. Who have much in common.

<a href="#ftnt_ref15" id="ftnt15">[15]</a> "The
Millionaire Mind" By Thomas J. Stanley inspired this line. Millionaires
fix they don't throw.

<a href="#ftnt_ref16" id="ftnt16">[16]</a> This of
course is impossible when the language and libraries change like dirty
diapers every year.

<a href="#ftnt_ref17" id="ftnt17">[17]</a> Jonathan
the Seychelles giant tortoise is 190+ years old. 'Nough said.

<a href="#ftnt_ref18" id="ftnt18">[18]</a> If ever
in the Fort Lauderdale area say Hi.

<a href="#ftnt_ref19" id="ftnt19">[19]</a><https://www.gnu.org/gnu/rms-Lisp.en.html>


<a href="#ftnt_ref20" id="ftnt20">[20]</a> Sadly the
GPL can be abused by corporations to do great harm, entire languages and communities have been paralized by using the GPL to virus developer's software using their tools to prevent commercial use which harms all developers. I no longer use the GPL (BSD/MIT now)  because of this and still very much respect the idea of keeping the tools free. On the ground though that is not what is being done, GPL was and often is used to poison our tools to prevent Vertical development (see next note).

<a href="#ftnt_ref21" id="ftnt21">[21]</a> "Vertical"
development is for example using CLOG to create a customer website or
niche app and "Horizontal" development is general libraries and
tools.

<a href="#ftnt_ref22" id="ftnt22">[22]</a> I could have
a list of self deprecating things about myself and all would be true,
but part of my goal with CLOG is attracting people to Common Lisp and to
do that with this generation of whippersnappers it means whiz bang
graphicy stuff, fresh and new stuff too focus them long enough to see it is worth it to work hard and Lisp will payoff. How You
ask? With CLOG. You can already write "Horizontal Apps" faster and in
more creative ways than ever before. That means you can turn apps into
cash on an individual and corporate level and that means more developers in Common Lisp and that means more of another generation of people to advance the human race.

<a href="#ftnt_ref23" id="ftnt23">[23]</a> Almost
every culture has clogs of one type or another and where there are clogs there are cloggers dancing! A CLOGer loves to program and do cool stuff, for me writing software is my artistic outlet and I want to share my art and inspire others to use it and make their own.
