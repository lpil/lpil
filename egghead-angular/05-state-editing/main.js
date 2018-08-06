var app = angular.module('myApp', []);

app.controller('MainCtrl', function($scope) {
  $scope.categories = [
    {"id": 0, "name": "Development"},
    {"id": 1, "name": "Design"},
    {"id": 2, "name": "Exercise"},
    {"id": 3, "name": "Humor"}
  ];

  $scope.bookmarks = [
    {"id": 0, "title": "AngularJS", "url": "http://angularjs.org", "category": "Development" },
    {"id": 1, "title": "Egghead.io", "url": "http://angularjs.org", "category": "Development" },
    {"id": 2, "title": "A List Apart", "url": "http://alistapart.com/", "category": "Design" },
    {"id": 3, "title": "One Page Love", "url": "http://onepagelove.com/", "category": "Design" },
    {"id": 4, "title": "MobilityWOD", "url": "http://www.mobilitywod.com/", "category": "Exercise" },
    {"id": 5, "title": "Robb Wolf", "url": "http://robbwolf.com/", "category": "Exercise" },
    {"id": 6, "title": "Senor Gif", "url": "http://memebase.cheezburger.com/senorgif", "category": "Humor" },
    {"id": 7, "title": "Wimp", "url": "http://wimp.com", "category": "Humor" },
    {"id": 8, "title": "Dump", "url": "http://dump.com", "category": "Humor" }
  ];

  $scope.setCurrentCategory = function setCurrentCategory(category) {
    $scope.currentCategory = category;
    $scope.isCreating = false;
    $scope.isEditing = false;
  };

  $scope.isCurrentCategory = function isCurrentCategory(category) {
    if ($scope.currentCategory) {
      return category.name === $scope.currentCategory.name;
    }
  };

  /***********
   *  State  *
   ***********/
  $scope.isCreating = false;
  $scope.isEditing = false;

  var startCreating = function startCreating() {
    $scope.isCreating = true;
    $scope.isEditing = false;
    resetNewBookmarkForm();
  };
  var startEditing = function startEditing() {
    $scope.isCreating = false;
    $scope.isEditing = true;
  };

  var cancelCreating = function cancelCreating() {
    $scope.isCreating = false;
  };
  var cancelEditing = function cancelEditing() {
    $scope.isEditing = false;
  };

  $scope.startCreating = startCreating;
  $scope.startEditing = startEditing;
  $scope.cancelCreating = cancelCreating;
  $scope.cancelEditing = cancelEditing;

  // "Creating" can be shown if we are on a category AND if not editing.
  var shouldShowCreating = function shouldShowCreating() {
    if ($scope.currentCategory) { return !$scope.isEditing; }
  };

  // "Editing" can be shown if editing and not creating
  var shouldShowEditing = function shouldShowEditing() {
    return $scope.isEditing && !$scope.isCreating;
  };

  $scope.shouldShowCreating = shouldShowCreating;
  $scope.shouldShowEditing = shouldShowEditing;

  /*******************
   *  Bookmark CRUD  *
   *******************/

  var resetNewBookmarkForm = function resetNewBookmarkForm() {
    $scope.newBookmark = {
      title: '',
      url: '',
      category: $scope.currentCategory.name
    };
  };
  
  var createBookmark = function createBookmark(bookmark) {
    bookmark.id = $scope.bookmarks.length;
    $scope.bookmarks.push(bookmark);

    resetNewBookmarkForm();
  };
  $scope.createBookmark = createBookmark;

  $scope.editedBookmark = null;
  var setEditedBookmark = function setEditedBookmark(bookmark) {
    $scope.editedBookmark = angular.copy(bookmark);
  };
  $scope.setEditedBookmark = setEditedBookmark;

  var updateBookmark = function updateBookmark(bookmark) {
    var i = $scope.bookmarks.length;
    while (i--) {
      if ($scope.bookmarks[i].id === bookmark.id) {
        $scope.bookmarks[i] = bookmark;
        $scope.editedBookmark = null;
        $scope.isEditing = false;

        return;
      }
    }
  };
  $scope.updateBookmark = updateBookmark;

  var isSelectedBookmark = function isSelectedBookmark(bookmark) {
    return $scope.editedBookmark && $scope.editedBookmark.id === bookmark.id;
  };
  $scope.isSelectedBookmark = isSelectedBookmark;
});
