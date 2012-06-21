require './react'

include React

sess = Session.new
sess.set('user', '')

user = ObservableValue.new { sess.get('user') }
upUser = ObservableValue.new { user.get.upcase }
Observer.new { puts upUser.get }

sess.set('user', 'jcheng')
Context.flush
sess.set('user', 'jjallaire')
Context.flush
