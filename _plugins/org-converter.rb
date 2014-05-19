
module Jekyll
  class OrgConverter < Converter
    safe true
    priority :low

    def matches(ext)
      ext =~ /^\.org$/i
    end

    def output_ext(ext)
      ".html"
    end

    def convert(content)
      inread, inwrite = IO.pipe
      outread, outwrite = IO.pipe

      inwrite.write content
      inwrite.close
      emacs_execution_string = "emacs -Q --batch -L " \
        "_vendor/org-8.2.6/lisp/ -l _lib/org-convert.el -f compile-org-file"
      org_pid = spawn(emacs_execution_string, :in => inread, :out => outwrite, :err => :out)
      inread.close
      outwrite.close
      Process.wait(org_pid)

      out_content = outread.read
      outread.close
      out_content
    end
  end
end
