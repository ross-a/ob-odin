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

note: wraps up code in main and imports "core:fmt" if there is no main proc() 
