angular.module('categories.bookmarks', [
    'categories.bookmarks.create',
    'categories.bookmarks.edit',
    'myApp.models.categories',
    'myApp.models.bookmarks'
])

.config(function($stateProvider) {
  $stateProvider
    .state('myApp.categories.bookmarks', {
      url: 'categories/:category',
      views: {
        'bookmarks@': {
          templateUrl: 'app/categories/bookmarks/bookmarks.tmpl.html',
          controller: 'BookmarksCtrl'
        }
      }
    });
})

.controller('BookmarksCtrl', function($scope, $stateParams) {
  $scope.currentCategoryName = $stateParams.category;
});
