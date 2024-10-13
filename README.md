# TCSVFile - CSV File Handling Class for Delphi12

The `TCSVFile` class is designed to simplify reading, writing, and manipulating CSV files in Delphi/Pascal applications. It offers a variety of methods to parse, search, modify, and export CSV data. This class supports custom delimiters, encodings, and provides extensive functionality for managing changes and performing searches within the CSV file.

## Key Features

- **Customizable CSV Parsing**: Supports different delimiters (e.g., commas, semicolons) and encodings (UTF-8 by default). Option to include or exclude a header row.
- **Change Tracking**: Keeps track of changes made to individual cells with a dedicated change history for each cell.
- **Event Handling**: Provides several events, such as `OnAfterReadRow`, `OnBeforeReadCell`, and `OnProgress`, allowing customized behavior during parsing and processing.
- **Data Export**: CSV data can be exported to multiple formats including XML, JSON, and XLSX.
- **Efficient Searching**: Offers functions to search and replace data within specific columns, with support for regular expressions.
- **Data Manipulation**: Supports operations such as sorting, copying columns, inserting rows/columns, and swapping rows/columns.

## Properties

- **`Autoselect`**: Enables or disables automatic selection behavior when interacting with the data.
- **`Cell[Row, Col]`**: Access or modify the content of a specific cell.
- **`Changed[Row, Col]`**: Check or mark if a cell has been modified.
- **`ColCount[Row]`**: Returns the number of columns in a specified row.
- **`Data`**: Provides access to the underlying list of CSV data.
- **`Encoding`**: Specifies the file encoding (default is UTF-8).
- **`LineBreakStyle`**: Defines the line break style for the CSV file (Windows, Unix, etc.).
- **`RowCount`**: Returns the total number of rows in the CSV file.
- **`Separator`**: Specifies the column separator used in the CSV file.
- **`Size`**: Returns the size of the CSV data (number of cells).
- **`UseFastParsing`**: Enables or disables optimized parsing for large files.
- **`UseHeader`**: Indicates whether the first row is treated as a header.

## Methods

- **`Create(AEncoding: TEncoding = nil; ASeparator: Char = ',')`**: Constructor that initializes the CSV file with the specified encoding and delimiter (defaults: UTF-8 and comma).
- **`Clear()`**: Clears all data and resets change tracking.
- **`ParseCSVContent(Content: string)`**: Parses CSV data from a string.
- **`LoadFromFile(FileName: string)`**: Loads CSV data from a file.
- **`SaveToFile(FileName: string)`**: Saves the current CSV data to a file.
- **`Search(SearchText: string; Column: Integer = -1; RegexOn: Boolean = False)`**: Searches for a specific text in a column or the entire file.
- **`ExportAsJSON(Filename: string)`**: Exports the CSV data as a JSON file.
- **`ExportAsXLSX(Filename: string)`**: Exports the CSV data to an Excel file.
- **`InsertRow(ColumnCount: Integer; Position: Integer)`**: Inserts a new row at a specified position.
- **`DeleteRow(Index: Integer)`**: Deletes a row from the CSV file.
- **`CombineCols(Col: Integer; BlankString: string = '')`**: Combines all cells in a column into a single cell.
- **`DetectDataType(Value: string)`**: Detects the data type of a cell’s value (e.g., string, number).
- **`ClearChanges()`**: Clears the list of tracked changes.

## Events

- **`OnAfterReadRow`**: Triggered after reading a row from the CSV.
- **`OnBeforeReadCell`**: Triggered before reading each cell.
- **`OnProgress`**: Reports the progress of operations like file loading or parsing.

## Example Usage

```delphi
var
  CSVFile: TCSVFile;
begin
  CSVFile := TCSVFile.Create(TEncoding.UTF8, ';');  // Create a CSV file with UTF-8 encoding and semicolon separator
  CSVFile.LoadFromFile('example.csv');  // Load data from a CSV file
  CSVFile.Cell[1, 2] := 'Updated Value';  // Modify a specific cell
  CSVFile.SaveToFile('output.csv');  // Save the updated data to a new file
  CSVFile.Free;
end;
```


## Additional Notes

- **Fast Parsing**: When working with large files, you can enable `UseFastParsing` to improve performance.
- **Multi-line CSV Support**: The class supports CSV files where individual cells contain multi-line text.
- **Data Validation**: The class includes basic support for detecting data types in CSV fields, which can be extended as needed.

This class provides robust CSV handling capabilities for developers working with Delphi12, making it easy to manage, modify, and export structured data in a variety of formats.

