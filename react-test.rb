require './react'

class Observer
  def initialize(&proc)
    @proc = proc
    run
  end

  def run
    ctx = React::Context.new
    ctx.on_invalidate do
      run
    end
    ctx.run &@proc
  end
end

sess = React::Session.new
user = React::ObservableValue.new { sess.get('user') }
upUser = React::ObservableValue.new { (user.get||'').upcase }
Observer.new { puts upUser.get }

sess.set('user', 'jcheng')
React::Context.flush
sess.set('user', 'jjallaire')
React::Context.flush
