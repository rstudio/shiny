require './react'

include React

def print_observable_value(obsVal)
  Observer.new { puts obsVal.value }
end

sess = Session.new
sess.set('user', '')

user = ObservableValue.new { sess.get('user') }
user_caps = ObservableValue.new { user.value.upcase }

# This will print the value not just once, but every
# time the value changes
print_observable_value(user_caps)

sess.set('user', 'jcheng')
Context.flush  # pay no attention to the man behind the curtain
sess.set('user', 'jjallaire')
sess.set('user', 'jwpaulson')
Context.flush
