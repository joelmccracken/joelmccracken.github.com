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
      require 'socket'
      TCPSocket.open 'emacs-org-compiler', 9876 do |socket|
        socket.puts "Length: #{content.bytesize}"
        socket.write content
        socket.read
      end
    end
  end
end
