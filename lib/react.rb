module React

  class Context

    private

      @@next_id = 0
      @@current_context = nil
      @@pending_invalidate = []

    public

      def self.current!
        return current || raise("No current context")
      end

      def self.current
        @@current_context
      end

      attr_reader :id

      def initialize
        # The ID can used to identify/sort/dedupe contexts
        @id = @@next_id += 1

        # Indicates whether this context is invalidated, i.e. its
        # callbacks have been called or are about to be called
        @invalidated = false

        # List of callbacks to be called after invalidation
        @callbacks = []
      end

      # Run a block with this context as the current context. The
      # original current context will be restored after the block
      # is executed.
      def run
        old_ctx = @@current_context
        @@current_context = self
        begin
          return yield
        ensure
          @@current_context = old_ctx
        end
      end

      def invalidate
        return if @invalidated

        @invalidated = true
        @@pending_invalidate << self
      end

      # Register a callback to be called after this context is
      # invalidated (or immediately if it's already invalidated).
      # The callback takes one argument, the context.
      def on_invalidate(&callback)
        if @invalidated
          callback.call(self)
        else
          @callbacks << callback
        end
      end

      def execute_callbacks
        @callbacks.each {|callback| callback.call(self)}
      end

      # Execute all callbacks on invalidated contexts. Will do this
      # repeatedly if the callbacks themselves cause more invalidations.
      def self.flush
        while !@@pending_invalidate.empty?
          contexts = @@pending_invalidate
          @@pending_invalidate = []

          contexts.each {|context| context.execute_callbacks}
        end
      end
  end

  class Session
    def initialize
      # Key is variable name, value is variable value
      @values = Hash.new
      # Key is variable name, value is { Context IDs => Contexts }
      @dependencies = Hash.new
    end

    def get(name)
      cur_ctx = React::Context.current!
      @dependencies[name] = @dependencies[name] || Hash.new
      if !@dependencies[name].has_key?(cur_ctx.id)
        @dependencies[name][cur_ctx.id] = cur_ctx
        cur_ctx.on_invalidate do
          @dependencies[name].delete(cur_ctx.id)
        end
      end

      return @values[name]
    end

    def set(name, value)
      if @values.has_key?(name) && @values[name] == value
        return
      end

      @values[name] = value
      if @dependencies[name]
        @dependencies[name].each_value {|ctx| ctx.invalidate}
      end
    end
  end

  # Stores (and caches) a single dependent value in a context
  class ObservableValue
    def initialize(&valueProc)
      @valueProc = valueProc
      
      @dependencies = Hash.new
      @initialized = false
    end

    def value
      if !@initialized
        @initialized = true
        update_value
      end

      cur_ctx = React::Context.current!
      @dependencies[cur_ctx.id] = cur_ctx
      cur_ctx.on_invalidate do
        @dependencies.delete cur_ctx.id
      end
      @value
    end

    private
      def update_value
        old_value = @value
  
        ctx = Context.new
        ctx.on_invalidate do
          update_value
        end
        ctx.run do
          @value = @valueProc.call
        end

        if old_value != @value
          @dependencies.each_value {|dep_ctx| dep_ctx.invalidate}
        end
      end
  end

  # Runs the given proc whenever its dependencies change
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

end