$(function() {

    var clip = new ZeroClipboard($("#copy-button"));
    clip.on( 'mouseup', function ( client, args ) { alert( "Code copied to clipboard" );} );

    $('#closebutton').click(function() { window.close();});

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
	    $(el).find("li span").each(function() { result.push($(this).text()) });
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


})

