require './shiny'
require 'digest/sha1'
require 'digest/md5'

shinyapp = ShinyApp.new

input1 = React::ObservableValue.new {
  (shinyapp.session.get('input1') || '') + (shinyapp.session.get('addnewline') ? "\n" : '')
}

shinyapp.define_output('md5_hash') do
  Digest::MD5.hexdigest(input1.value)
end

shinyapp.define_output('sha1_hash') do
  Digest::SHA1.hexdigest(input1.value)
end

shinyapp.run
