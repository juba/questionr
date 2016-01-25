var bind_sortable = function() {

  /* JQuery sortable */
  $(".sortable" ).sortable({
    placeholder: "ui-state-highlight",
    forcePlaceholderSize: true
  });
  $(".sortable").disableSelection();

  /* Shiny sortable binding */
    
  var sortableBinding = new Shiny.InputBinding();

  $.extend(sortableBinding, {
	find: function(scope) {
	    return $(scope).find(".sortable");
	},
	getValue: function(el) {
	    var result = [];
	    $(el).find("li span.level").each(function() { result.push($(this).text()) });
	    return result;
	},
	subscribe: function(el, callback) {
	    $(el).on("sortupdate.sortableBinding", function(e) {
		    callback();
	    });
	},
	unsubscribe: function(el) {
	    $(el).off(".sortableBinding");
	}
    });

    Shiny.inputBindings.register(sortableBinding);

};

bind_sortable();
$(document).on("shiny:inputchanged", bind_sortable);

