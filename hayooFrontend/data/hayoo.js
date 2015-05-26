function makeAutocomplete() {
    var cache = {};
    $( "#hayoo" ).autocomplete({
        minLength: 2,
        source: function( request, response ) {
            var term = request.term;
            if ( term in cache ) {
                response( cache[ term ] );
                return;
            }

            $.getJSON( "/autocomplete", request, function( data, status, xhr ) {
                cache[ term ] = data;
                response( data );
            });
        }
    });
}


function makeMores (target) {
    var showChar = 150;
    var maxChar = 200;
    var ellipsestext = "...";
    var moretext = "more";
    var lesstext = "less";
    var t = target ? target : document;
    $(t).find('.more').each(function() {
        var content = $(this).html();
        var textContent = $(this).text();

        if(textContent.length > maxChar) {

            var c = textContent.substr(0, showChar);


            var html = '<div class="preview">' + c + '<span class="moreelipses">'+ellipsestext+'</span>&nbsp;<a href="" class="morelink">'+moretext+'</a></span></div>'
                     + '<div class="content" style="display: none;"">' + content + '<a href="" class="lesslink">'+lesstext+'</a></div>';
            $(this).html(html);
        }

    });

    $(t).find(".morelink").click(function(){
        $(this).parent().hide()
        $(this).parent().next().show()
        return false;
    });
    $(t).find(".lesslink").click(function(){
        $(this).parent().hide()
        $(this).parent().prev().show()
        return false;
    });
}

var page = 1
function addPage(reset) {
    if (page < 20) {
        params = {
            "query": currentQuery
        }
        $.get("/ajax/" + page + "/", params, function(d){
            var d1 = $(d);
            makeMores(d1);
            $("#results").append(d1)
            page += 1
        }).always(reset)
    }
}

function makeNextPage() {
    $('#next-page-button').click(function () {
        var btn = $(this)
        btn.button('loading')
        addPage(function () {
            btn.button('reset')
        });
    });
}

$().ready(function() {
    makeAutocomplete()

    makeMores()

    makeNextPage()

    $("#hayoo").focus()
});
