require 'eventmachine'
require 'evma_httpserver'
require 'em-websocket'
require 'pathname'
require 'json'
require './react'


class WebServer < EM::Connection
  include EM::HttpServer

  def post_init
    super
    no_environment_strings

    @basepath = File.join(Dir.pwd, 'www')
  end

  def resolve_path(path)
    # It's not a valid path if it doesn't start with /
    return nil if path !~ /^\//

    abspath = File.join(@basepath, "./#{path}")
    # Resolves '..', etc.
    abspath = Pathname.new(abspath).cleanpath.to_s

    return false if abspath[0...(@basepath.size + 1)] != @basepath + '/'
    return false if !File.exist?(abspath)

    return abspath
  end

  def process_http_request
    # the http request details are available via the following instance variables:
    #   @http_protocol
    #   @http_request_method
    #   @http_cookie
    #   @http_if_none_match
    #   @http_content_type
    #   @http_path_info
    #   @http_request_uri
    #   @http_query_string
    #   @http_post_content
    #   @http_headers

    response = EM::DelegatedHttpResponse.new(self)

    path = @http_path_info
    path = '/index.html' if path == '/'

    resolved_path = resolve_path(path)

    if !resolved_path
      response.status = 404
      response.content_type 'text/html'
      response.content = '<h1>404 Not Found</h1>'
    else
      response.status = 200
      response.content_type case resolved_path
      when /\.html?$/
        'text/html'
      when /\.js$/
        'text/javascript'
      when /\.css$/
        'text/css'
      when /\.png$/
        'image/png'
      when /\.jpg$/
        'image/jpeg'
      when /\.gif$/
        'image/gif'
      end
      response.content = File.read(resolved_path)
    end
    response.send_response
  end
end

def run_shiny_app(shinyapp)
  EventMachine.run do
    EventMachine.start_server '0.0.0.0', 8100, WebServer
    puts "Listening on port 8100"
    
    EventMachine::WebSocket.start(:host => '0.0.0.0', :port => 8101) do |ws|
      shinyapp.websocket = ws
      ws.onclose { exit(0) }
      ws.onmessage do |msg|
        puts "RECV: #{msg}"

        msg_obj = JSON.parse(msg)
        case msg_obj['method']
        when 'init'
          msg_obj['data'].each do |k, v|
            shinyapp.session.set(k, v)
          end
          React::Context.flush
          shinyapp.instantiate_outputs
        when 'update'
          msg_obj['data'].each do |k, v|
            shinyapp.session.set(k, v)
          end
        end

        React::Context.flush
      end
    end
  end
end

class ShinyApp
  attr_reader :session

  def initialize
    @session = React::Session.new
    @outputs = {}
  end

  def websocket=(value)
    @websocket = value
  end

  def define_output(name, &proc)
    @outputs[name] = proc
  end

  def instantiate_outputs
    @outputs.keys.each do |name|
      proc = @outputs.delete(name)
      React::Observer.new do
        value = proc.call
        msg = {}
        msg[name] = value
        puts "SEND: #{JSON.generate(msg)}"
        @websocket.send(JSON.generate(msg))
      end
    end
  end

  def run
    run_shiny_app self
  end
end

