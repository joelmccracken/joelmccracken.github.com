
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

    def ensure_server!
      @server_pid ||=
        begin
          emacs_execution_string = "emacs -Q --daemon=org-convert-daemon -L " \
            "_vendor/org-8.2.6/lisp/ -l _lib/org-convert.el -f start-compile-server"
          server_pid = spawn(emacs_execution_string)

          at_exit do
            spawn("emacsclient -s org-convert-daemon -e '(kill-emacs)'")
          end

          sleep 3 # give the process a few secs to warm up

          server_pid
        end
    end

    def convert(content)
      require 'socket'
      ensure_server!
      TCPSocket.open 'localhost', 9876 do |socket|
        socket.puts "Length: #{content.bytesize}"
        socket.write content
        socket.read
      end
    end
  end
end
