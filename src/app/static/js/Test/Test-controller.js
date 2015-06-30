'use strict';

angular.module('myapp')
  .controller('TestController', ['$scope', '$modal', 'resolvedTest', 'Test',
    function ($scope, $modal, resolvedTest, Test) {

      $scope.Tests = resolvedTest;

      $scope.create = function () {
        $scope.clear();
        $scope.open();
      };

      $scope.update = function (id) {
        $scope.Test = Test.get({id: id});
        $scope.open(id);
      };

      $scope.delete = function (id) {
        Test.delete({id: id},
          function () {
            $scope.Tests = Test.query();
          });
      };

      $scope.save = function (id) {
        if (id) {
          Test.update({id: id}, $scope.Test,
            function () {
              $scope.Tests = Test.query();
              $scope.clear();
            });
        } else {
          Test.save($scope.Test,
            function () {
              $scope.Tests = Test.query();
              $scope.clear();
            });
        }
      };

      $scope.clear = function () {
        $scope.Test = {
          
          "name": "",
          
          "id": ""
        };
      };

      $scope.open = function (id) {
        var TestSave = $modal.open({
          templateUrl: 'Test-save.html',
          controller: 'TestSaveController',
          resolve: {
            Test: function () {
              return $scope.Test;
            }
          }
        });

        TestSave.result.then(function (entity) {
          $scope.Test = entity;
          $scope.save(id);
        });
      };
    }])
  .controller('TestSaveController', ['$scope', '$modalInstance', 'Test',
    function ($scope, $modalInstance, Test) {
      $scope.Test = Test;

      

      $scope.ok = function () {
        $modalInstance.close($scope.Test);
      };

      $scope.cancel = function () {
        $modalInstance.dismiss('cancel');
      };
    }]);
