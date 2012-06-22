(function() {

  var $ = jQuery;

  var ShinyApp = function() {
    this.$socket = null;
    this.$bindings = {};
    this.$values = {};
    this.$pendingMessages = [];
  };

  (function() {

    this.connect = function(initialInput) {
      if (this.$socket)
        throw "Connect was already called on this application object";

      this.$socket = this.createSocket();
      this.$initialInput = initialInput;
    };

    this.createSocket = function () {
      var self = this;

      var socket = new WebSocket('ws://' + window.location.hostname + ':8101/events');
      socket.onopen = function() {
        console.log('connected to websocket');

        socket.send(JSON.stringify({
          method: 'init',
          data: self.$initialInput
        }))

        while (self.$pendingMessages.length) {
          var msg = self.$pendingMessages.shift();
          socket.send(msg);
        }
      };
      socket.onmessage = function(e) {
        self.dispatchMessage(e.data);
      };
      socket.onclose = function() {
        $(document.body).addClass('disconnected');
      };
      return socket;
    };

    this.sendInput = function(values) {
      var msg = JSON.stringify({
        method: 'update',
        data: values
      });

      if (this.$socket.readyState == WebSocket.CONNECTING) {
        this.$pendingMessages.push(msg);
      }
      else {
        this.$socket.send(msg);
      }
    };

    this.receiveOutput = function(name, value) {
      var oldValue = this.$values[name];
      this.$values[name] = value;
      if (oldValue === value)
        return;

      var binding = this.$bindings[name];
      if (binding) {
        binding.onValueChange(value);
      }

      return value;
    };

    this.dispatchMessage = function(msg) {
      var msgObj = JSON.parse(msg);
      for (key in msgObj) {
        this.receiveOutput(key, msgObj[key]);
      }
    };

    this.bind = function(id, binding) {
      if (!id)
        throw "Can't bind an element with no ID";
      if (this.$bindings[id])
        throw "Duplicate binding for ID " + id;
      this.$bindings[id] = binding;
      return binding;
    };
  }).call(ShinyApp.prototype);


  var LiveTextBinding = function(el) {
    this.el = el;
  };

  (function() {
    this.onValueChange = function(data) {
      $(this.el).text(data);
    };
  }).call(LiveTextBinding.prototype);


  $(function() {

    var shinyapp = window.shinyapp = new ShinyApp();

    $('.live-text').each(function() {
      shinyapp.bind(this.id, new LiveTextBinding(this));
    });

    var initialValues = {};
    $('input').each(function() {
      var input = this;
      var name = input.name;
      var value = $(input).val();
      // TODO: validate name is non-blank, and no duplicates
      // TODO: If submit button is present, don't send anything
      //   until submit button is pressed
      initialValues[name] = value;
      $(input).keyup(function() {
        var data = {};
        data[name] = $(input).val();
        shinyapp.sendInput(data);
      });
    });

    shinyapp.connect(initialValues);
  });
})();
