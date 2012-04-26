## based off of
# https://gist.github.com/642739

module Jekyll
  require 'slim'
  
  class SlimGenerator < Generator
    safe true
    priority :low

    def generate(site)
      src_root = site.config['source']
      dest_root = site.config['destination']
      slim_ext = /\.slim$/i
      
      # static_files have already been filtered against excludes, etc.
      site.static_files.each do |sf|
        next if not sf.path =~ slim_ext
        slim_path = sf.path
        html_path = slim_path.gsub(slim_ext, '.html')#.gsub(src_root, dest_root)
        html_dir = File.dirname(html_path)
        html_dir_relative = html_dir.gsub(src_root, '')
        html_name = File.basename(html_path)

        FileUtils.mkdir_p(html_dir)

        if needs_gen?(slim_path, html_path)
          out = Slim::Template.new(slim_path).render;
          f = File.write(html_path, out)
        end
      end
    end
    def needs_gen? src, dest
      return true unless File.exists?(dest)
      File.stat(src).mtime.to_i > File.stat(dest).mtime.to_i
    end
  end
end

