# .sanitise_universal snapshot

    Code
      cat(EventStudy:::.sanitise_universal(.prose_fixture), sep = "\n")
    Output
      Returns rose 5% & fell <10> for A_B firms; cost $3 #tag {x} ~approx ^power \path "quoted" 'single'.

# .sanitise_for_pdf snapshot

    Code
      cat(EventStudy:::.sanitise_for_pdf(.prose_fixture), sep = "\n")
    Output
      Returns rose 5\% \& fell <10> for A\_B firms; cost \$3 \#tag \{x\} \textasciitilde{}approx \textasciicircum{}power \textbackslash{}path "quoted" 'single'.

# .sanitise_for_word snapshot

    Code
      cat(EventStudy:::.sanitise_for_word(.prose_fixture), sep = "\n")
    Output
      Returns rose 5% &amp; fell &lt;10&gt; for A_B firms; cost $3 #tag {x} ~approx ^power \path &quot;quoted&quot; &#39;single&#39;.

# .sanitise_prose dispatcher snapshot (pdf + word)

    Code
      cat(EventStudy:::.sanitise_prose(.prose_fixture, "pdf"), sep = "\n")
    Output
      Returns rose 5\% \& fell <10> for A\_B firms; cost \$3 \#tag \{x\} \textasciitilde{}approx \textasciicircum{}power \textbackslash{}path "quoted" 'single'.

---

    Code
      cat(EventStudy:::.sanitise_prose(.prose_fixture, "word"), sep = "\n")
    Output
      Returns rose 5% &amp; fell &lt;10&gt; for A_B firms; cost $3 #tag {x} ~approx ^power \path &quot;quoted&quot; &#39;single&#39;.

