# ob-odin
org-babel functions for odin evaluation

---

in .emacs
<pre class="src">(require 'ob-odin)</pre>

in some .org file
<pre class="src src-odin">#+begin_src odin
  fmt.println("Hello World");
#+end_src
</pre>

then C-c C-c to check, compile and run to get results

note: import "core:fmt" gets included by default and code is wrapped in a main proc()
