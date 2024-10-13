# TCSVFile - CSV File Handling Class for Delphi12

The `TCSVFile` class is designed to simplify reading, writing, and manipulating CSV files in Delphi/Pascal applications. It offers a variety of methods to parse, search, modify, and export CSV data. This class supports custom delimiters, encodings, and provides extensive functionality for managing changes and performing searches within the CSV file.  
For more detailed information, [visit the official documentation (only german at the moment)](https://doku.tmedia-agentur.de/TCSVFile/).


## Key Features

- **Customizable CSV Parsing**: Supports different delimiters (e.g., commas, semicolons) and encodings (UTF-8 by default). Option to include or exclude a header row.
- **Change Tracking**: Keeps track of changes made to individual cells with a dedicated change history for each cell.
- **Event Handling**: Provides several events, such as `OnAfterReadRow`, `OnBeforeReadCell`, and `OnProgress`, allowing customized behavior during parsing and processing.
- **Data Export**: CSV data can be exported to multiple formats including XML, JSON, and XLSX.
- **Efficient Searching**: Offers functions to search and replace data within specific columns, with support for regular expressions.
- **Data Manipulation**: Supports operations such as sorting, copying columns, inserting rows/columns, and swapping rows/columns.



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

