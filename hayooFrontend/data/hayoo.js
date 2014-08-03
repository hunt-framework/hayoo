
var Application;
(function (Application) {
    (function (Controller) {
        var HayooController = (function () {
            function HayooController($scope, $http, $location, $sce, $routeParams) {
                this.$scope = $scope;
                this.$http = $http;
                this.$location = $location;
                this.$sce = $sce;
		
		var hayoo = this;
		hayoo.results = [];	// valid results
		hayoo.error = [];	// error messages
		
		hayoo.page = 0;		// current page
		hayoo.offset = 20;	// current results per page 
		hayoo.count = 0;	// current queries results overall
		
		hayoo.isLastPage = (hayoo.page+1) > ((hayoo.count / hayoo.offset)-1);
		hayoo.isFirstPage = hayoo.page === 0;
		
		$scope.$on('$routeChangeSuccess', function() {
		   hayoo.query = $routeParams.query;
		   var searchUrl = 'json?query=' + hayoo.query + "&page=" + hayoo.page;
		   $http.get(searchUrl).success(function (data) {
		      // todo: handle error messages as soon as they are available in json format
		      hayoo.results = data.result;
		      hayoo.count = data.count;
		  });   
		});		

		this.search = function() {
		   var query = hayoo.query === undefined ? "" : "/" + hayoo.query;
		   hayoo.$location.path("/search"+ query);
		}
		
		this.previous = function() {
		   hayoo.page = hayoo.isFirstPage ? 0 : hayoo.page-1;
		   hayoo.search();
		}
		
		this.next = function() {
		   hayoo.page = hayoo.isLastPage ? hayoo.page : hayoo.page+1;
		   hayoo.search();
		}
            }
            HayooController.$inject = ['$scope', '$http', '$location', '$sce', '$routeParams'];
            return HayooController;
        })();
        Controller.HayooController = HayooController;
    })(Application.Controller || (Application.Controller = {}));
    var Controller = Application.Controller;
})(Application || (Application = {}));

var app = angular.module('Application', [
  'ngRoute'
]);

app.config(['$routeProvider', function ($routeProvider) {
    $routeProvider.
        when('/search/', { 
            templateUrl: './index.html',
	    constroller: 'HayooController'
        }).
        when('/search/:query', { 
            templateUrl: './results.html',
	    constroller: 'HayooController'
        }).
        when('/examples', { 
            templateUrl: './examples.html',
        }).
        when('/about', { 
            templateUrl: './about.html',
        }).
        otherwise({
            redirectTo: '/search'
        });
}]);

app.controller('HayooController', Application.Controller.HayooController);

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


function makeMores () {
    var showChar = 150;
    var maxChar = 200;
    var ellipsestext = "...";
    var moretext = "more";
    var lesstext = "less";
    $('.more').each(function() {
        var content = $(this).html();
        var textContent = $(this).text();

        if(textContent.length > maxChar) {

            var c = textContent.substr(0, showChar);
            

            var html = '<div class="preview">' + c + '<span class="moreelipses">'+ellipsestext+'</span>&nbsp;<a href="" class="morelink">'+moretext+'</a></span></div>'
                     + '<div class="content" style="display: none;"">' + content + '<a href="" class="lesslink">'+lesstext+'</a></div>';
            $(this).html(html);
        }

    });

    $(".morelink").click(function(){
        $(this).parent().hide()
        $(this).parent().next().show()
        return false;
    });
    $(".lesslink").click(function(){
        $(this).parent().hide()
        $(this).parent().prev().show()
        return false;
    });
}

var page = 0
function addPage(reset) {
    if (page < 20) {
        params = {
            "query": currentQuery
        }
        $.get("/ajax/" + page + "/", params, function(d){
            $("#results").append(d)
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


