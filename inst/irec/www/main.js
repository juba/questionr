$(function() {

    var clip = new ZeroClipboard($("#copy-button"));
    clip.on( 'mouseup', function ( client, args ) { alert( "Code copied to clipboard" );} );

    $('#closebutton').click(function() { window.close();});

})
