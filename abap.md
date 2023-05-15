## About this Repository

This repository is a compilation of the most important ABAP coding basics and should serve as a reference for the most relevant aspects. It is based on experience and sections of detailed documentation, which are can be accessed driectly via a separate file. 

## Topics

- [Declarations](#declaration)
   - [Type](#declrarations_type)   
   - [Variable (and Structure + Table)](#declaration_var) 
   - [Field Symbol](#declarations_fs) 
   - [Object and Reference](#declarations_fs) 
- [Table Operation](#table)
- [Use of References](#reference)

## Declarations

### Type 

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

### Object and Reference

```ABAP
" intitialization
DATA: lo_object       TYPE REF TO lcl_class,
      lv_ref_variable TYPE REF TO data_element.

" declaration
lo_object = NEW lcl_class( ).
```


## Table Operation

### Table Information

```ABAP
" number of lines 
lv_lines_itab = lines( lt_table ).

" check if specific line exists
IF line_exists( lt_table[ variable = 'value' ] ). 
   " line exists where column 'variable == 'value'
ENDIF.   
```

### Insert/Append Data

```ABAP
" append values directly
lt_table = VALUE #( variable1 = 'column1' variable2 = 'column2' ).

" insert initial line that can be filled via assigned field symbol
INSERT INITIAL LINE TO lt_table ASSIGNING <fs_structure>.
<fs_structure>-variable = 'column1'

" append initial line that can be filled via assigned field symbol
APPEND INITIAL LINE TO lt_table ASSIGNING <fs_structure>.
<fs_structure>-variable = 'column1'.
```

### Loop At/Read Table 

```ABAP
LOOP AT lt_table ASSIGNING <fs_structure> { WHERE variable = 'value' } 
   " do processing with <fs_structure>. Changing values in fs_structure changes them also in itab!
ENDLOOP.
```

```ABAP
" read directly (WARNING: Line must exist - if not an error occurs) 
ls_structure = lt_table8[ variable = 'value' ].

" read table with index
READ TABLE lt_table INDEX 1 ASSIGING <fs_structure>.

" read table with key field 
READ TABLE lt_table WITH TABLE KEY variable = 'value' ASSIGING <fs_structure>.
```

## Use of References 

```ABAP
" declaration and assignment of value 
DATA: lv_ref_variable TYPE REF TO data_element.

GET REFERENCE OF lv_variable INTO lv_ref_variable.

" assign field symbol to use reference
lv_ref_variable->* = <fs_variable>. 
```


