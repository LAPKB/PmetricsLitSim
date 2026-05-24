$(function() {
  Shiny.addCustomMessageHandler("toggleCondition", function(message) {
    Shiny.setInputValue(message.name, message.value);
  });

  $(document).on("keydown", ".dataTable input", function(event) {
    if (event.key !== "Tab") {
      return;
    }

    event.preventDefault();

    var $input = $(this);
    var $cell = $input.closest("td");
    var $row = $cell.closest("tr");
    var $table = $row.closest("table");
    var cellIndex = $cell.index();
    var rowIndex = $row.index();
    var $allRows = $table.find("tbody tr");
    var numCols = $row.find("td").length;
    var numRows = $allRows.length;

    $input.blur();

    setTimeout(function() {
      var newCellIndex;
      var newRowIndex;

      if (event.shiftKey) {
        newCellIndex = cellIndex - 1;
        newRowIndex = rowIndex;
        if (newCellIndex < 0) {
          newCellIndex = numCols - 1;
          newRowIndex = rowIndex - 1;
          if (newRowIndex < 0) {
            newRowIndex = numRows - 1;
          }
        }
      } else {
        newCellIndex = cellIndex + 1;
        newRowIndex = rowIndex;
        if (newCellIndex >= numCols) {
          newCellIndex = 0;
          newRowIndex = rowIndex + 1;
          if (newRowIndex >= numRows) {
            newRowIndex = 0;
          }
        }
      }

      var $targetRow = $allRows.eq(newRowIndex);
      var $targetCell = $targetRow.find("td").eq(newCellIndex);
      $targetCell.trigger("dblclick");
    }, 50);
  });
});