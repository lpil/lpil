angular.module('categories', [
    'myApp.models.categories'
])

.config(function($stateProvider) {
  $stateProvider
    .state('myApp.categories', {
      url: '/',
      views: {
        'categories@': {
          controller: 'CategoriesCtrl',
          templateUrl: 'app/categories/categories.tmpl.html'
        },
        'bookmarks@': {
          controller: 'BookmarksCtrl',
          templateUrl: 'app/categories/bookmarks/bookmarks.tmpl.html'
        }
      }
    });
})

.controller('CategoriesCtrl', function ($scope) {
  $scope;
});
