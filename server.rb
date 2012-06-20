require 'em-websocket'
require 'rack'

class RapportApp

  # Rack entry point
  def call(env)
    return [
      200,
      {'Content-Type' => 'text/html'},
      ["Hi"]
    ]
  end

end

rapp = RapportApp.new

Rack::Server.new(
  :app => rapp,
  :Port => 8113,
  :server => 'webrick').start
