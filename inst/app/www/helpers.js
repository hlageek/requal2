  $(document).on('click', '.value-button', function() {
    var buttonId = $(this).attr('id');
    var dataValue = $(this).data('value');
    // Add a timestamp to ensure the value changes with each click
    // var timestamp = new Date().getTime();
    Shiny.setInputValue(buttonId, dataValue, {priority: "event"});
  });