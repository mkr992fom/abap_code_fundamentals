## About this Repository

This repository is a compilation of the most important ABAP coding basics and should serve as a reference for the most relevant aspects. It is based on experience and sections of detailed documentation, which are documented in a separate file. 

## Topics

- [Declarations](#declarations)
   - [Variable](##declarations_var) 

## Declarations

### Types 

```ABAP
" regular type 
TYPES: t_type TYPE data_type.

" structure type
TYPES:
  BEGIN OF t_structure_type,   
    variable1 TYPE data_type,
    variable2 TYPE data_type,
  END OF t_structure_type.
  
" table type 
TYPES: tt_table_type TYPE TABLE OF t_structure_type.
```

### Variable (and Stucture + Table) 

Typical initialization of variables: 

```ABAP
" variable 
DATA: lv_variable TYPE data_type

" structure
DATA: ls_structure TYPE t_structure_type.
      
" table 
DATA: lt_table TYPE TABLE OF t_structure_type,
      lt_table TYPE tt_table_type.
```

Additional information:

```ABAP
" initialize with value
DATA: lv_variable TYPE data_type VALUE 'value'.

" initialization directly in coding 
DATA(lv_variable) = lo_object->method_returning( ).
```

### Field Symbol

```ABAP
" variable field symbol
FIELD-SYMBOLS: <fs_variable>  TYPE data_element.

" structure field symbol
FIELD-SYMBOLS: <fs_structure> TYPE t_structure_type,
               <fs_structure> TYPE LINE OF tt_table_type.

" table field symbol
FIELD-SYMBOLS: <fs_table>     TYPE tt_table_type.
```

### Objects and References

```ABAP
" intitialization
DATA: lo_object       TYPE REF TO lcl_class,
      lv_ref_variable TYPE REF TO data_element.

" declaration
lo_object = NEW lcl_class( ).
```


