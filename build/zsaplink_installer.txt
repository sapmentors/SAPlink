*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
REPORT  ZSAPLINK_INSTALLER.

CLASS ZCX_SAPLINK DEFINITION
  INHERITING FROM CX_STATIC_CHECK
  CREATE PUBLIC
  .
public section.
*"* public components of class ZCX_SAPLINK
*"* do not include other source files here!!!

  constants EXISTING type SOTR_CONC value '000C29BC2BF91ED18ED3F65AA0FCB872'. "#EC NOTEXT
  constants SYSTEM_ERROR type SOTR_CONC value '000C29BC2BF91ED18ED3F65AA0FD5872'. "#EC NOTEXT
  constants NOT_AUTHORIZED type SOTR_CONC value '000C29BC2BF91ED18ED3F65AA0FCF872'. "#EC NOTEXT
  constants ERROR_MESSAGE type SOTR_CONC value '000C29BC2BF91ED18ED3F65AA0FC9872'. "#EC NOTEXT
  constants ZCX_SAPLINK type SOTR_CONC value '000C29BC2BF91ED18ED3F65AA0FD7872'. "#EC NOTEXT
  data MSG type STRING value '44F7518323DB08BC02000000A7E42BB6'. "#EC NOTEXT .
  constants NOT_FOUND type SOTR_CONC value '000C29BC2BF91ED18ED3F65AA0FD1872'. "#EC NOTEXT
  constants LOCKED type SOTR_CONC value '000C29BC2BF91ED18ED3F65AA0FCD872'. "#EC NOTEXT
  constants NO_PLUGIN type SOTR_CONC value '000C29BC2BF91ED18ED6493728DB99D2'. "#EC NOTEXT
  data OBJECT type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !MSG type STRING default '44F7518323DB08BC02000000A7E42BB6'
      !OBJECT type STRING optional .
*"* protected components of class ZCX_SAPLINK
*"* do not include other source files here!!!
protected section.
private section.
*"* private components of class ZCX_SAPLINK
*"* do not include other source files here!!!
ENDCLASS.
CLASS ZSAPLINK DEFINITION
  CREATE PUBLIC
  ABSTRACT
  .
public section.
*"* public components of class ZSAPLINK
*"* do not include other source files here!!!

  data NUGGET_LEVEL type INT4 read-only value 0. "#EC NOTEXT .

  class-methods GETOBJECTINFOFROMIXMLDOC
    importing
      !IXMLDOCUMENT type ref to IF_IXML_DOCUMENT
    exporting
      !OBJTYPENAME type STRING
      !OBJNAME type STRING .
  class-methods CONVERTSTRINGTOIXMLDOC
    importing
      value(XMLSTRING) type STRING
    returning
      value(IXMLDOCUMENT) type ref to IF_IXML_DOCUMENT .
  class-methods CONVERTIXMLDOCTOSTRING
    importing
      !IXMLDOCUMENT type ref to IF_IXML_DOCUMENT
    returning
      value(XMLSTRING) type STRING .
  methods CREATEOBJECTFROMIXMLDOC
  abstract
    importing
      !IXMLDOCUMENT type ref to IF_IXML_DOCUMENT
      !DEVCLASS type DEVCLASS default '$TMP'
      !OVERWRITE type FLAG optional
    returning
      value(NAME) type STRING
    raising
      ZCX_SAPLINK .
  methods CREATEIXMLDOCFROMOBJECT
  abstract
    returning
      value(IXMLDOCUMENT) type ref to IF_IXML_DOCUMENT
    raising
      ZCX_SAPLINK .
  methods CREATESTRINGFROMOBJECT
    returning
      value(STRING) type STRING
    raising
      ZCX_SAPLINK .
  methods CONSTRUCTOR
    importing
      !NAME type STRING .
  methods UPLOADXML
  final
    importing
      !XMLDATA type STRING .
  class-methods GETPLUGINS
    changing
      value(OBJECTTABLE) type TABLE .
  methods CHECKEXISTS
  abstract
    returning
      value(EXISTS) type FLAG .
  methods VALUEHELP
    importing
      !I_OBJTYPE type STRING
    returning
      value(E_OBJNAME) type STRING .
  class-methods CHECKOBJECT
    importing
      !I_IXMLDOCUMENT type ref to IF_IXML_DOCUMENT
    exporting
      !E_OBJTYPE type STRING
      !E_OBJNAME type STRING
      !E_PLUGINEXISTS type FLAG
      !E_OBJECTEXISTS type FLAG
      !E_TARGETOBJECT type ref to ZSAPLINK .
protected section.
*"* protected components of class ZSAPLINK
*"* do not include other source files here!!!

  data OBJNAME type STRING .
  data IXML type ref to IF_IXML .
  data XMLDOC type ref to IF_IXML_DOCUMENT .

  methods DELETEOBJECT
  abstract
    raising
      ZCX_SAPLINK .
  class-methods SETATTRIBUTESFROMSTRUCTURE
    importing
      !NODE type ref to IF_IXML_ELEMENT
      !STRUCTURE type DATA .
  class-methods GETSTRUCTUREFROMATTRIBUTES
    importing
      !NODE type ref to IF_IXML_ELEMENT
      !PRESERVEVERSION type FLAG optional
    changing
      !STRUCTURE type DATA .
  methods CREATEXMLSTRING
  final
    returning
      value(XML) type STRING .
  class-methods BUILDTABLEFROMSTRING
    importing
      !SOURCE type STRING
    returning
      value(SOURCETABLE) type TABLE_OF_STRINGS .
  class-methods BUILDSOURCESTRING
    importing
      !SOURCETABLE type RSWSOURCET optional
      !PAGETABLE type O2PAGELINE_TABLE optional
    returning
      value(SOURCESTRING) type STRING .
  methods GETOBJECTTYPE
  abstract
    returning
      value(OBJECTTYPE) type STRING .
  methods CREATEOTRFROMNODE
    importing
      value(NODE) type ref to IF_IXML_ELEMENT
      !DEVCLASS type DEVCLASS default '$TMP'
    exporting
      !CONCEPT type SOTR_TEXT-CONCEPT
    raising
      ZCX_SAPLINK .
  methods CREATENODEFROMOTR
    importing
      !OTRGUID type SOTR_CONC
    returning
      value(NODE) type ref to IF_IXML_ELEMENT .
private section.
*"* private components of class ZSAPLINK
*"* do not include other source files here!!!

  types:
    BEGIN OF t_objecttable,
           classname TYPE string,
           object TYPE ko100-object,
           text TYPE ko100-text,
         END OF t_objecttable .

  data STREAMFACTORY type ref to IF_IXML_STREAM_FACTORY .
  data XMLDATA type STRING .
  data:
    objecttable TYPE TABLE OF t_objecttable .
ENDCLASS.
CLASS ZSAPLINK_OO DEFINITION
  INHERITING FROM ZSAPLINK
  CREATE PUBLIC
  ABSTRACT
  .
public section.
*"* public components of class ZSAPLINK_OO
*"* do not include other source files here!!!
  type-pools ABAP .
  type-pools SEOP .
  type-pools SEOR .
  type-pools SEOS .
  type-pools SEOT .
  type-pools SEOX .

  constants C_XML_KEY_FRIENDS type STRING value 'friends'. "#EC NOTEXT
  constants C_XML_KEY_INHERITANCE type STRING value 'inheritance'. "#EC NOTEXT
  constants C_XML_KEY_SOTR type STRING value 'sotr'. "#EC NOTEXT
  constants C_XML_KEY_SOTRTEXT type STRING value 'sotrText'. "#EC NOTEXT
protected section.
*"* protected components of class ZSAPLINK_OO
*"* do not include other source files here!!!

  constants C_XML_KEY_ALIAS_METHOD type STRING value 'aliasMethod'. "#EC NOTEXT
  constants C_XML_KEY_CLSDEFERRD type STRING value 'typeClasDef'. "#EC NOTEXT
  constants C_XML_KEY_FORWARDDECLARATION type STRING value 'forwardDeclaration'. "#EC NOTEXT
  constants C_XML_KEY_INTDEFERRD type STRING value 'typeIntfDef'. "#EC NOTEXT
  constants C_XML_KEY_TYPEPUSAGE type STRING value 'typeUsage'. "#EC NOTEXT

  methods CREATE_ALIAS_METHOD
    changing
      !XT_ALIASES_METHOD type SEOO_ALIASES_R .
  methods CREATE_CLSDEFERRD
    changing
      !XT_CLSDEFERRDS type SEOT_CLSDEFERRDS_R .
  methods CREATE_INTDEFERRD
    changing
      !XT_INTDEFERRDS type SEOT_INTDEFERRDS_R .
  methods CREATE_OTR
    importing
      value(NODE) type ref to IF_IXML_ELEMENT
      !DEVCLASS type DEVCLASS default '$TMP'
    exporting
      !CONCEPT type SOTR_TEXT-CONCEPT
    raising
      ZCX_SAPLINK .
  methods CREATE_TYPEPUSAGE
    changing
      !XT_TYPEPUSAGES type SEOT_TYPEPUSAGES_R .
  methods GET_ALIAS_METHOD
    importing
      !IT_METHODS type ABAP_METHDESCR_TAB
    changing
      !XO_ROOTNODE type ref to IF_IXML_ELEMENT .
  methods GET_CLSDEFERRD
    changing
      !XO_ROOTNODE type ref to IF_IXML_ELEMENT .
  methods GET_INTDEFERRD
    changing
      !XO_ROOTNODE type ref to IF_IXML_ELEMENT .
  methods GET_OTR
    importing
      !OTRGUID type SOTR_CONC
    returning
      value(NODE) type ref to IF_IXML_ELEMENT .
  methods GET_TYPEPUSAGE
    changing
      !XO_ROOTNODE type ref to IF_IXML_ELEMENT .
private section.
*"* private components of class ZSAPLINK_OO
*"* do not include other source files here!!!
ENDCLASS.
CLASS ZSAPLINK_CLASS DEFINITION
  INHERITING FROM ZSAPLINK_OO
  CREATE PUBLIC
  .
public section.
*"* public components of class ZSAPLINK_CLASS
*"* do not include other source files here!!!

  methods CHECKEXISTS
    redefinition .
  methods CREATEIXMLDOCFROMOBJECT
    redefinition .
  methods CREATEOBJECTFROMIXMLDOC
    redefinition .
protected section.
*"* protected components of class ZSAPLINK_CLASS
*"* do not include other source files here!!!

  constants C_XML_KEY_METHOD_DOCUMENTATION type STRING value 'methodDocumentation'. "#EC NOTEXT
  constants C_XML_KEY_TEXTELEMENT type STRING value 'textElement'. "#EC NOTEXT
  constants C_XML_KEY_TEXTPOOL type STRING value 'textPool'. "#EC NOTEXT
  constants C_XML_KEY_CLASS_DOCUMENTATION type STRING value 'classDocumentation'. "#EC NOTEXT
  constants C_XML_KEY_LANGUAGE type STRING value 'language'. "#EC NOTEXT
  constants C_XML_KEY_OBJECT type STRING value 'OBJECT'. "#EC NOTEXT
  constants C_XML_KEY_SPRAS type STRING value 'SPRAS'. "#EC NOTEXT
  constants C_XML_KEY_TEXTLINE type STRING value 'textLine'. "#EC NOTEXT

  methods CREATE_DOCUMENTATION .
  methods CREATE_METHOD_DOCUMENTATION
    importing
      !NODE type ref to IF_IXML_ELEMENT .
  methods CREATE_SECTIONS .
  methods CREATE_TEXTPOOL .
  methods FINDIMPLEMENTINGCLASS
    importing
      !METHODNAME type STRING
      !STARTCLASS type STRING optional
    returning
      value(CLASSNAME) type STRING .
  methods GET_DOCUMENTATION
    changing
      !ROOTNODE type ref to IF_IXML_ELEMENT .
  methods GET_METHOD_DOCUMENTATION
    importing
      !METHOD_KEY type SEOCPDKEY
    changing
      !ROOTNODE type ref to IF_IXML_ELEMENT .
  methods GET_SECTIONS
    changing
      !ROOTNODE type ref to IF_IXML_ELEMENT .
  methods GET_TEXTPOOL
    changing
      !ROOTNODE type ref to IF_IXML_ELEMENT .

  methods DELETEOBJECT
    redefinition .
  methods GETOBJECTTYPE
    redefinition .
private section.
*"* private components of class ZSAPLINK_CLASS
*"* do not include other source files here!!!
ENDCLASS.
CLASS ZSAPLINK_PROGRAM DEFINITION
  INHERITING FROM ZSAPLINK
  FINAL
  CREATE PUBLIC
  .
public section.
*"* public components of class ZSAPLINK_PROGRAM
*"* do not include other source files here!!!

  methods CHECKEXISTS
    redefinition .
  methods CREATEIXMLDOCFROMOBJECT
    redefinition .
  methods CREATEOBJECTFROMIXMLDOC
    redefinition .
  methods CREATESTRINGFROMOBJECT
    redefinition .
protected section.
*"* protected components of class ZSAPLINK_PROGRAM
*"* do not include other source files here!!!

  methods DELETEOBJECT
    redefinition .
  methods GETOBJECTTYPE
    redefinition .
private section.
*"* private components of class ZSAPLINK_PROGRAM
*"* do not include other source files here!!!

  methods GET_SOURCE
    returning
      value(PROGSOURCE) type RSWSOURCET .
  methods UPDATE_WB_TREE .
  methods CREATE_TEXTPOOL
    importing
      !TEXTPOOLNODE type ref to IF_IXML_ELEMENT .
  methods DEQUEUE_ABAP
    raising
      ZCX_SAPLINK .
  methods GET_TEXTPOOL
    returning
      value(TEXTNODE) type ref to IF_IXML_ELEMENT .
  methods CREATE_DOCUMENTATION
    importing
      !DOCNODE type ref to IF_IXML_ELEMENT .
  methods CREATE_SOURCE
    importing
      !SOURCE type TABLE_OF_STRINGS
      !ATTRIBS type TRDIR .
  methods ENQUEUE_ABAP
    raising
      ZCX_SAPLINK .
  methods GET_DOCUMENTATION
    returning
      value(DOCNODE) type ref to IF_IXML_ELEMENT .
  methods TRANSPORT_COPY
    importing
      !AUTHOR type SYUNAME
      !DEVCLASS type DEVCLASS
    raising
      ZCX_SAPLINK .
  methods GET_DYNPRO
    returning
      value(DYNP_NODE) type ref to IF_IXML_ELEMENT .
  methods CREATE_DYNPRO
    importing
      !DYNP_NODE type ref to IF_IXML_ELEMENT .
  methods GET_PFSTATUS
    returning
      value(PFSTAT_NODE) type ref to IF_IXML_ELEMENT .
  methods CREATE_PFSTATUS
    importing
      !PFSTAT_NODE type ref to IF_IXML_ELEMENT .
ENDCLASS.
CLASS ZCX_SAPLINK IMPLEMENTATION.
  method CONSTRUCTOR.
  CALL METHOD SUPER->CONSTRUCTOR
  EXPORTING
  TEXTID = TEXTID
  PREVIOUS = PREVIOUS
  .
   IF textid IS INITIAL.
     me->textid = ZCX_SAPLINK .
   ENDIF.
  me->MSG = MSG .
  me->OBJECT = OBJECT .
  endmethod.
ENDCLASS.
CLASS ZSAPLINK IMPLEMENTATION.
  method BUILDSOURCESTRING.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
  data sTemp type string.
  data pageLine type O2PAGELINE.

    if sourceTable is not initial.
      loop at sourceTable into sTemp.
        concatenate sourceString sTemp CL_ABAP_CHAR_UTILITIES=>NEWLINE
          into sourceString.
      endloop.
    elseif pageTable is not initial.
      loop at pageTable into pageLine.
        concatenate sourceString pageLine-line
          CL_ABAP_CHAR_UTILITIES=>NEWLINE
          into sourceString.
      endloop.
    endif.

* remove extra newline characters for conversion comparison consistency
    shift sourceString left deleting leading
      CL_ABAP_CHAR_UTILITIES=>NEWLINE.
    shift sourceString right deleting trailing
      CL_ABAP_CHAR_UTILITIES=>NEWLINE.
    shift sourceString left deleting leading space.
  endmethod.
  method BUILDTABLEFROMSTRING.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
    split source at CL_ABAP_CHAR_UTILITIES=>NEWLINE
      into table sourceTable.
  endmethod.
  method CHECKOBJECT.
    DATA l_objtable LIKE objecttable.
    DATA l_objline  LIKE LINE OF objecttable.

    CLEAR: e_objtype, e_objname, e_pluginexists, e_objectexists.
    CALL METHOD zsaplink=>getobjectinfofromixmldoc
      EXPORTING
        ixmldocument = i_ixmldocument
      IMPORTING
        objtypename  = e_objtype
        objname      = e_objname.

    CALL METHOD zsaplink=>getplugins( CHANGING objecttable = l_objtable ).

    READ TABLE l_objtable INTO l_objline WITH KEY object = e_objtype.

    IF sy-subrc = 0.
      e_pluginexists = 'X'.
      CREATE OBJECT e_targetobject
        TYPE
        (l_objline-classname)
        EXPORTING
          name = e_objname.

      e_objectexists = e_targetobject->checkexists( ).
    ENDIF.

  endmethod.
  method CONSTRUCTOR.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/

*  data meTypeDescr type ref to CL_ABAP_TYPEDESCR.
*  clear className.
*
*  objName = name.
*  meTypeDescr = CL_ABAP_TYPEDESCR=>DESCRIBE_BY_OBJECT_REF( me ).
*  className = meTypeDescr->get_relative_name( ).

    objName = name.
    translate objName to upper case.

    ixml = cl_ixml=>create( ).
    xmlDoc = ixml->create_document( ).
    streamFactory = ixml->CREATE_STREAM_FACTORY( ).
  endmethod.
  method CONVERTIXMLDOCTOSTRING.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
    data _ixml type ref to if_ixml.
    data _encoding   type ref to if_ixml_encoding.
    data _streamFactory type ref to IF_IXML_STREAM_FACTORY.
    data _outputStream type ref to IF_IXML_OSTREAM.
    data _renderer type ref to IF_IXML_RENDERER.
    data _tempString  type string.
    data _tempStringx type xstring.
    data _printXMLDoc type ref to cl_xml_document.
    data _rc type sysubrc.

    _ixml = cl_ixml=>create( ).
    _encoding = _ixml->create_encoding(
        byte_order    = if_ixml_encoding=>co_none
        character_set = 'utf-8' ).
    _streamFactory = _ixml->CREATE_STREAM_FACTORY( ).
    _outputStream = _streamFactory->create_ostream_xstring( _tempStringx ).
    _outputstream->set_encoding( encoding = _encoding ).
    _renderer = _ixml->CREATE_RENDERER(
                  DOCUMENT = ixmlDocument
                  OSTREAM  = _outputStream
                ).
    _renderer->SET_NORMALIZING( ).
    _rc = _renderer->render( ).
    create object _printXMLDoc.
    _rc = _printXMLDoc->parse_string( _tempString ).

    CALL FUNCTION 'ECATT_CONV_XSTRING_TO_STRING'
      EXPORTING
        im_xstring  = _tempstringx
        im_encoding = 'UTF-8'
      IMPORTING
        ex_string   = _tempstring.

    xmlString = _tempString.
  endmethod.
  method CONVERTSTRINGTOIXMLDOC.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
    data ixml type ref to if_ixml.
    data streamFactory type ref to IF_IXML_STREAM_FACTORY.
    data iStream type ref to if_ixml_istream.
    data ixmlParser type ref to if_ixml_parser.
    data xmlDoc type ref to if_ixml_document.

    " Make sure to convert Windows Line Break to Unix as
    " this linebreak is used to get a correct import
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf
      IN xmlString WITH cl_abap_char_utilities=>newline.

    ixml = cl_ixml=>create( ).
    xmlDoc = ixml->create_document( ).
    streamFactory = ixml->CREATE_STREAM_FACTORY( ).
    iStream = streamFactory->CREATE_ISTREAM_STRING( xmlString ).
    iXMLParser = iXML->create_parser(  stream_factory = streamFactory
                                       istream        = iStream
                                       document       = xmlDoc ).
    iXMLParser->parse( ).
    ixmlDocument = xmlDoc.
  endmethod.
  method CREATENODEFROMOTR.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
    DATA rootnode TYPE REF TO if_ixml_element.
    DATA txtnode TYPE REF TO if_ixml_element.
    DATA rc TYPE sysubrc.

    DATA sotrheader TYPE sotr_head.
    DATA sotrtextline TYPE sotr_text.
    DATA sotrtexttable TYPE TABLE OF sotr_text.

    DATA _ixml TYPE REF TO if_ixml.
    DATA _xmldoc TYPE REF TO if_ixml_document.

    CALL FUNCTION 'SOTR_GET_CONCEPT'
      EXPORTING
        concept        = otrguid
      IMPORTING
        header         = sotrheader
      TABLES
        entries        = sotrtexttable
      EXCEPTIONS
        no_entry_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

    sotrheader-paket = '$TMP'. "change devclass to $TMP for exports

* Create xml doc
*  _ixml = cl_ixml=>create( ).
*  _xmldoc = _ixml->create_document( ).
*  streamfactory = _ixml->create_stream_factory( ).

* Create parent node
    rootnode = xmldoc->create_element( zsaplink_oo=>c_xml_key_sotr ). "OTR object type
    CLEAR sotrheader-concept.                                 "ewH:33
    setattributesfromstructure( node = rootnode structure = sotrheader ).

* Create nodes for texts
    LOOP AT sotrtexttable INTO sotrtextline.
      txtnode = xmldoc->create_element( zsaplink_oo=>c_xml_key_sotrtext ).
      CLEAR: sotrtextline-concept, sotrtextline-object.       "ewH:33
      setattributesfromstructure(
        node = txtnode structure = sotrtextline ).
      rc = rootnode->append_child( txtnode ).
    ENDLOOP.

    node = rootnode.

  endmethod.
  method CREATEOTRFROMNODE.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
    DATA txtnode TYPE REF TO if_ixml_element.
    DATA filter TYPE REF TO if_ixml_node_filter.
    DATA iterator TYPE REF TO if_ixml_node_iterator.

    DATA sotrheader TYPE sotr_head.
    DATA sotrtextline TYPE sotr_text.
    DATA sotrtexttable TYPE TABLE OF sotr_text.
    DATA sotrpaket TYPE sotr_pack.

* get OTR header info
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = sotrheader.

* get OTR text info
    filter = node->create_filter_name( zsaplink_oo=>c_xml_key_sotrText ).
    iterator = node->create_iterator_filtered( filter ).
    txtnode ?= iterator->get_next( ).

    WHILE txtnode IS NOT INITIAL.
      CLEAR sotrtextline.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = txtnode
        CHANGING
          structure = sotrtextline.
      CLEAR: sotrtextline-concept, sotrtextline-object.       "ewH:33
      APPEND sotrtextline TO sotrtexttable.
      txtnode ?= iterator->get_next( ).
    ENDWHILE.

* ewH:issue 33--> in 6.40 and above, you cannot pass a default concept
*  (otr) guid, so we will always create new
*  CALL FUNCTION 'SOTR_GET_CONCEPT'
*    EXPORTING
*      concept              = sotrHeader-concept
**   IMPORTING
**     HEADER               =
**   TABLES
**     ENTRIES              =
*   EXCEPTIONS
*     NO_ENTRY_FOUND       = 1
*     OTHERS               = 2
*            .
*  IF sy-subrc <> 1.
**   delete OTR if exists already
*    CALL FUNCTION 'SOTR_DELETE_CONCEPT'
*      EXPORTING
*        concept                     = sotrHeader-concept
*     EXCEPTIONS
*       NO_AUTHORIZATION            = 1
*       NO_ENTRY_FOUND              = 2. "who cares
**       CONCEPT_USED                = 3
**       NO_MASTER_LANGUAGE          = 4
**       NO_SOURCE_SYSTEM            = 5
**       NO_TADIR_ENTRY              = 6
**       ERROR_IN_CORRECTION         = 7
**       USER_CANCELLED              = 8
**       OTHERS                      = 9
**              .
*    if sy-subrc = 1.
*      raise exception type zcx_saplink
*        exporting textid = zcx_saplink=>not_authorized.
*    endif.
*  ENDIF.


    DATA objecttable TYPE sotr_objects.
    DATA objecttype TYPE LINE OF sotr_objects.
* Retrieve object type of OTR
    CALL FUNCTION 'SOTR_OBJECT_GET_OBJECTS'
      EXPORTING
        object_vector    = sotrheader-objid_vec
      IMPORTING
        OBJECTS          = objecttable
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.

    READ TABLE objecttable INTO objecttype INDEX 1.

* create OTR
    sotrpaket-paket = devclass.
    CALL FUNCTION 'SOTR_CREATE_CONCEPT'
      EXPORTING
        paket                               = sotrpaket
        crea_lan                            = sotrheader-crea_lan
        alias_name                          = sotrheader-alias_name
*      CATEGORY                            =
        object                              = objecttype
        entries                             = sotrtexttable
*     FLAG_CORRECTION_ENTRY               =
*     IN_UPDATE_TASK                      =
*      CONCEPT_DEFAULT                     = sotrHeader-concept "ewH:33
      IMPORTING
        concept                             = concept         "ewH:33
      EXCEPTIONS
        package_missing                     = 1
        crea_lan_missing                    = 2
        object_missing                      = 3
        paket_does_not_exist                = 4
        alias_already_exist                 = 5
        object_type_not_found               = 6
        langu_missing                       = 7
        identical_context_not_allowed       = 8
        text_too_long                       = 9
        error_in_update                     = 10
        no_master_langu                     = 11
        error_in_concept_id                 = 12
        alias_not_allowed                   = 13
        tadir_entry_creation_failed         = 14
        internal_error                      = 15
        error_in_correction                 = 16
        user_cancelled                      = 17
        no_entry_found                      = 18
        OTHERS                              = 19
              .
    IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  endmethod.
  method CREATESTRINGFROMOBJECT.
  endmethod.
  method CREATEXMLSTRING.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
  data streamFactory type ref to IF_IXML_STREAM_FACTORY.
  data outputStream type ref to IF_IXML_OSTREAM.
  data renderer type ref to IF_IXML_RENDERER.
  data tempString type string.
  data printXMLDoc type ref to cl_xml_document.
  data rc type sysubrc.

    streamFactory = ixml->CREATE_STREAM_FACTORY( ).
    outputStream = streamFactory->CREATE_OSTREAM_CSTRING( tempString ).
    renderer = ixml->CREATE_RENDERER(
      DOCUMENT = xmlDoc OSTREAM = outputStream ).
    rc = renderer->render( ).
    create object printXMLDoc.
    rc = printXMLDoc->parse_string( tempString ).
    xml = tempString.
  endmethod.
  method GETOBJECTINFOFROMIXMLDOC.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
  data rootNode type ref to IF_IXML_NODE.
  data rootAttr type ref to IF_IXML_NAMED_NODE_MAP.
  data AttrNode type ref to IF_IXML_NODE.
  data nodeName type string.

    rootNode ?= ixmlDocument->GET_ROOT_ELEMENT( ).

* get object type
    objTypeName = rootNode->GET_NAME( ).
    translate objTypeName to upper case.

* get object name
    rootAttr = rootNode->GET_ATTRIBUTES( ).
    AttrNode = rootAttr->GET_ITEM( 0 ).
    objName = AttrNode->GET_VALUE( ).
  endmethod.
  method GETPLUGINS.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
    DATA classlist TYPE seo_inheritances.
    DATA classline TYPE vseoextend.
    DATA classobject TYPE REF TO zsaplink.
    DATA objectline TYPE t_objecttable.
    DATA tabletypeline TYPE ko105.
    DATA tabletypesin TYPE TABLE OF ko105.
    DATA tabletypesout TYPE tr_object_texts.
    DATA tabletypeoutline TYPE ko100.
    DATA clsname TYPE string.
    DATA objtype TYPE trobjtype.

    REFRESH objecttable.

    SELECT * FROM vseoextend INTO TABLE classlist
      WHERE refclsname like 'ZSAPLINK%'
      AND version = '1'.

    LOOP AT classlist INTO classline.
      clsname = classline-clsname.
      TRY.
          CREATE OBJECT classobject
            TYPE
              (clsname)
            EXPORTING
              name      = 'foo'.
          objtype = classobject->getobjecttype( ).
        CATCH cx_root.
          CONTINUE.
      ENDTRY.
      CLEAR tabletypeline.
      REFRESH tabletypesin.

      tabletypeline-object = objtype.
      APPEND tabletypeline TO tabletypesin.

      CALL FUNCTION 'TRINT_OBJECT_TABLE'
        TABLES
          tt_types_in  = tabletypesin
          tt_types_out = tabletypesout.

      LOOP AT tabletypesout INTO tabletypeoutline.
        objectline-classname = clsname.
        MOVE-CORRESPONDING tabletypeoutline TO objectline.
        APPEND objectline TO objecttable.
      ENDLOOP.
    ENDLOOP.
  endmethod.
  method GETSTRUCTUREFROMATTRIBUTES.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
  data attributeList type ref to IF_IXML_NAMED_NODE_MAP.
  data nodeIterator type ref to IF_IXML_NODE_ITERATOR.
  data attributeNode type ref to if_ixml_node.
  data value type string.
  data name type string.
  field-symbols <value> type any.

    clear structure.
    attributeList = node->GET_ATTRIBUTES( ).
    nodeIterator = attributeList->create_iterator( ).
    attributeNode = nodeIterator->get_next( ).
    while attributeNode is not initial.
      name = attributeNode->get_name( ).
      if name = 'VERSION' and preserveVersion is initial. "ewh:issue 45
*    if name = 'VERSION'.
        value = '0'.
      else.
        value = attributeNode->get_value( ).
      endif.
      assign component name of structure structure to <value>.
      if sy-subrc = 0.
        <value> = value.
      endif.
      attributeNode = nodeIterator->get_next( ).
    endwhile.















*    .-"-.
*  .'=^=^='.
* /=^=^=^=^=\
*:^=SAPLINK=^;
*|^ EASTER  ^|
*:^=^EGG^=^=^:
* \=^=^=^=^=/
*  `.=^=^=.'
*    `~~~`
* Don't like the way we did something?
* Help us fix it!  Tell us what you think!
* http://saplink.org
  endmethod.
  method SETATTRIBUTESFROMSTRUCTURE.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
    data int type i.
    int = int.
    data structDescr type ref to cl_abap_structDescr.
    data aComponent type abap_compdescr.
    field-symbols <fieldValue> type any.
    data rc type sysubrc.
    data sName type string.
    data sValue type string.

    structDescr ?= cl_abap_structDescr=>describe_by_data( structure ).
    loop at structDescr->components into aComponent.
      assign component aComponent-name of structure
        structure to <fieldValue>.
      if sy-subrc = 0.
        sName = aComponent-name.
*      sValue = <fieldValue>.
*     for certain attributes, set to a standard for exporting
        case sName.
*        when 'VERSION'. "version should always export as inactive
*          sValue = '0'. "commented by ewH: issue 45
          when 'DEVCLASS'. "development class should always be $TMP
            sValue = '$TMP'.
          " Developer, Date and Time Metadata has to be removed to
          " not clutter diffs
          "
          " Meta Attributes for DDIC Types
          when 'AS4USER'.
            clear sValue.
          when 'AS4DATE'.
            clear sValue.
          when 'AS4TIME'.
            clear sValue.
          " Meta Attributes for PROG
          when 'CNAM'.
            clear sValue.
          when 'CDAT'.
            clear sValue.
          when 'UNAM'.
            clear sValue.
          when 'UDAT'.
            clear sValue.
          when 'VERN'.
            clear sValue.
          when 'SDATE'.
            clear sValue.
          when 'STIME'.
            clear sValue.
          when 'IDATE'.
            clear sValue.
          when 'ITIME'.
            clear sValue.
          " Meta Attributes for CLAS
          when 'AUTHOR'.
            clear sValue.
          when 'CREATEDON'.
            clear sValue.
          when 'CHANGEDBY'.
            clear sValue.
          when 'CHANGEDON'.
            clear sValue.
          when 'CHANGEDON'.
            clear sValue.
          when 'CHGDANYON'.
            clear sValue.
          when 'R3RELEASE'.
            clear sValue.
          when 'UUID'.
            clear sValue.
          " SOTR
          when 'CREA_NAME'.
            clear sValue.
          when 'CHAN_NAME'.
            clear sValue.
          when 'CREA_TSTUT'.
            clear sValue.
          when 'CHAN_TSTUT'.
            clear sValue.
          " MSAG
          when 'LASTUSER'.
            clear sValue.
          when 'LDATE'.
            clear sValue.
          when 'LTIME'.
            clear sValue.
          when others.
            sValue = <fieldValue>.
        endcase.
        if sValue is not initial.
          rc = Node->set_attribute( name = sName value = sValue ).
        endif.
      else.
* WHAT?>!??
      endif.
    endloop.
  endmethod.
  method UPLOADXML.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
  data iStream type ref to if_ixml_istream.
  data ixmlParser type ref to if_ixml_parser.

    iStream = streamFactory->CREATE_ISTREAM_STRING( xmlData ).
    iXMLParser = iXML->create_parser(  stream_factory = streamFactory
                                       istream        = iStream
                                       document       = XMLdoc ).
    iXMLParser->parse( ).

  endmethod.
  method VALUEHELP.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
  DATA l_object_type type  euobj-id.
  data objname(40) type c.

  l_object_type = i_objType.


    CALL FUNCTION 'REPOSITORY_INFO_SYSTEM_F4'
       EXPORTING
         object_type           = l_object_type
         object_name           = objname
         suppress_selection    = 'X'
         use_alv_grid          = ''
         without_personal_list = ''
       IMPORTING
         object_name_selected  = objname
       EXCEPTIONS
         cancel                = 1.

    e_objname = objname.
  endmethod.
ENDCLASS.
CLASS ZSAPLINK_OO IMPLEMENTATION.
  method CREATE_ALIAS_METHOD.
    DATA: filter TYPE REF TO if_ixml_node_filter,
          iterator TYPE REF TO if_ixml_node_iterator,
          node TYPE REF TO if_ixml_element.

    DATA: ls_alias_method  LIKE LINE OF xt_aliases_method.


    filter = xmldoc->create_filter_name( c_xml_key_alias_method ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CLEAR ls_alias_method.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = ls_alias_method.
      INSERT ls_alias_method INTO TABLE xt_aliases_method.
      node ?= iterator->get_next( ).
    ENDWHILE.

  endmethod.
  method CREATE_CLSDEFERRD.
    DATA: filter TYPE REF TO if_ixml_node_filter,
          iterator TYPE REF TO if_ixml_node_iterator,
          node TYPE REF TO if_ixml_element.

    DATA: ls_clsdeferrd  LIKE LINE OF xt_clsdeferrds.


    filter   = xmldoc->create_filter_name( c_xml_key_clsdeferrd ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).

    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = ls_clsdeferrd.
      APPEND ls_clsdeferrd TO xt_clsdeferrds.
      node ?= iterator->get_next( ).
    ENDWHILE.

  endmethod.
  method CREATE_INTDEFERRD.
    DATA: filter TYPE REF TO if_ixml_node_filter,
          iterator TYPE REF TO if_ixml_node_iterator,
          node TYPE REF TO if_ixml_element.

    DATA: ls_intdeferrd  LIKE LINE OF xt_intdeferrds.


    filter   = xmldoc->create_filter_name( c_xml_key_intdeferrd ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).

    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = ls_intdeferrd.
      APPEND ls_intdeferrd TO xt_intdeferrds.
      node ?= iterator->get_next( ).
    ENDWHILE.

  endmethod.
  method CREATE_OTR.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
    DATA txtnode TYPE REF TO if_ixml_element.
    DATA filter TYPE REF TO if_ixml_node_filter.
    DATA iterator TYPE REF TO if_ixml_node_iterator.

    DATA sotrheader TYPE sotr_head.
    DATA sotrtextline TYPE sotr_text.
    DATA sotrtexttable TYPE TABLE OF sotr_text.
    DATA sotrpaket TYPE sotr_pack.

* get OTR header info
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = sotrheader.

* get OTR text info
    filter = node->create_filter_name( c_xml_key_sotrText ).
    iterator = node->create_iterator_filtered( filter ).
    txtnode ?= iterator->get_next( ).

    WHILE txtnode IS NOT INITIAL.
      CLEAR sotrtextline.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = txtnode
        CHANGING
          structure = sotrtextline.
      CLEAR: sotrtextline-concept, sotrtextline-object.       "ewH:33
      APPEND sotrtextline TO sotrtexttable.
      txtnode ?= iterator->get_next( ).
    ENDWHILE.

* ewH:issue 33--> in 6.40 and above, you cannot pass a default concept
*  (otr) guid, so we will always create new
*  CALL FUNCTION 'SOTR_GET_CONCEPT'
*    EXPORTING
*      concept              = sotrHeader-concept
**   IMPORTING
**     HEADER               =
**   TABLES
**     ENTRIES              =
*   EXCEPTIONS
*     NO_ENTRY_FOUND       = 1
*     OTHERS               = 2
*            .
*  IF sy-subrc <> 1.
**   delete OTR if exists already
*    CALL FUNCTION 'SOTR_DELETE_CONCEPT'
*      EXPORTING
*        concept                     = sotrHeader-concept
*     EXCEPTIONS
*       NO_AUTHORIZATION            = 1
*       NO_ENTRY_FOUND              = 2. "who cares
**       CONCEPT_USED                = 3
**       NO_MASTER_LANGUAGE          = 4
**       NO_SOURCE_SYSTEM            = 5
**       NO_TADIR_ENTRY              = 6
**       ERROR_IN_CORRECTION         = 7
**       USER_CANCELLED              = 8
**       OTHERS                      = 9
**              .
*    if sy-subrc = 1.
*      raise exception type zcx_saplink
*        exporting textid = zcx_saplink=>not_authorized.
*    endif.
*  ENDIF.


    DATA objecttable TYPE sotr_objects.
    DATA objecttype TYPE LINE OF sotr_objects.
* Retrieve object type of OTR
    CALL FUNCTION 'SOTR_OBJECT_GET_OBJECTS'
      EXPORTING
        object_vector    = sotrheader-objid_vec
      IMPORTING
        OBJECTS          = objecttable
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.

    READ TABLE objecttable INTO objecttype INDEX 1.

* create OTR
    sotrpaket-paket = devclass.
    CALL FUNCTION 'SOTR_CREATE_CONCEPT'
      EXPORTING
        paket                               = sotrpaket
        crea_lan                            = sotrheader-crea_lan
        alias_name                          = sotrheader-alias_name
*      CATEGORY                            =
        object                              = objecttype
        entries                             = sotrtexttable
*     FLAG_CORRECTION_ENTRY               =
*     IN_UPDATE_TASK                      =
*      CONCEPT_DEFAULT                     = sotrHeader-concept "ewH:33
      IMPORTING
        concept                             = concept         "ewH:33
      EXCEPTIONS
        package_missing                     = 1
        crea_lan_missing                    = 2
        object_missing                      = 3
        paket_does_not_exist                = 4
        alias_already_exist                 = 5
        object_type_not_found               = 6
        langu_missing                       = 7
        identical_context_not_allowed       = 8
        text_too_long                       = 9
        error_in_update                     = 10
        no_master_langu                     = 11
        error_in_concept_id                 = 12
        alias_not_allowed                   = 13
        tadir_entry_creation_failed         = 14
        internal_error                      = 15
        error_in_correction                 = 16
        user_cancelled                      = 17
        no_entry_found                      = 18
        OTHERS                              = 19
              .
    IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  endmethod.
  method CREATE_TYPEPUSAGE.
    DATA: filter   TYPE REF TO if_ixml_node_filter,
          iterator TYPE REF TO if_ixml_node_iterator,
          node     TYPE REF TO if_ixml_element,
          source   TYPE string.


    DATA: ls_typepusage  LIKE LINE OF xt_typepusages.

*rrq comments Forward nodes are created in an old version of the
*create XML from object.  In that node, the only attribute set
*is the "TypeGroup".  All other attributes are hard coded on the
*create Object from XML .  To fix this and make it transparent to
*users, "forwaredDeclaration" nodes will be supported, and a new
*node will be added.
*if it is an old version XML document, forwardDeclarations nodes
*if it is a new version XML document, typeUsages nodes

    filter   = xmldoc->create_filter_name( c_xml_key_typepusage ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).

    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = ls_typepusage.
      APPEND ls_typepusage TO xt_typepusages.
      node ?= iterator->get_next( ).
    ENDWHILE.

* only check forwardDeclaration if typeUsages does not exist
* later version this will be removed
    IF xt_typepusages IS INITIAL.
      filter = xmldoc->create_filter_name( c_xml_key_forwarddeclaration ).
      iterator = xmldoc->create_iterator_filtered( filter ).
      node ?= iterator->get_next( ).

      WHILE node IS NOT INITIAL.
        CLEAR ls_typepusage.
        source = node->get_value( ).
        ls_typepusage-clsname = objname.
        ls_typepusage-version = '0'.
        ls_typepusage-tputype = '0'.
        ls_typepusage-explicit =  'X'.
        ls_typepusage-implicit = ''.
        ls_typepusage-typegroup = source.
        APPEND ls_typepusage TO xt_typepusages.
        node ?= iterator->get_next( ).
      ENDWHILE.
    ENDIF.

  endmethod.
  method GET_ALIAS_METHOD.
    DATA lo_alias  TYPE REF TO if_ixml_element.
    DATA ls_alias  TYPE seoaliases.
    DATA: l_rc     TYPE sy-subrc,
          ls_method LIKE LINE OF it_methods,
          ls_clsmethkey TYPE seocmpkey.

    LOOP AT it_methods INTO ls_method.
      ls_clsmethkey-clsname = objname.
      ls_clsmethkey-cmpname = ls_method-name.
      CLEAR ls_alias.
      CALL FUNCTION 'SEO_ALIAS_GET'
       EXPORTING
         cmpkey             = ls_clsmethkey
*       VERSION            = SEOC_VERSION_INACTIVE
       IMPORTING
         alias              = ls_alias
       EXCEPTIONS
         not_existing       = 1
         deleted            = 2
         OTHERS             = 3
               .
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ELSE.
        lo_alias = xmldoc->create_element( c_xml_key_alias_method ).
        setattributesfromstructure( node      = lo_alias
                                    structure = ls_alias ).
        l_rc = xo_rootnode->append_child( lo_alias ).
      ENDIF.
    ENDLOOP.

  endmethod.
  method GET_CLSDEFERRD.
    DATA: lt_clsdeferrds     TYPE seot_clsdeferrds_r,
          lo_clsdeferrds     TYPE REF TO if_ixml_element,
          ls_clsdeferrd      TYPE seot_typepusage_r.

    DATA: l_rc               TYPE sy-subrc,
          ls_classkey        TYPE seoclskey.

    ls_classkey-clsname = objname.

    CALL FUNCTION 'SEO_CLSDEFERRD_READ_ALL'
      EXPORTING
        cifkey            = ls_classkey
        version           = seoc_version_active
      IMPORTING
        CLASSDEFERREDS    = lt_clsdeferrds
      EXCEPTIONS
        clif_not_existing = 1
        OTHERS            = 2.

    LOOP AT lt_clsdeferrds INTO ls_clsdeferrd.
      lo_clsdeferrds = xmldoc->create_element( c_xml_key_clsdeferrd ).
      setattributesfromstructure( node      = lo_clsdeferrds
                                  structure = ls_clsdeferrd ).
      l_rc = xo_rootnode->append_child( lo_clsdeferrds ).
    ENDLOOP.
  endmethod.
  method GET_INTDEFERRD.
    DATA: lt_intdeferrds     TYPE seot_intdeferrds_r,
          lo_intdeferrds     TYPE REF TO if_ixml_element,
          ls_intdeferrd      TYPE seot_intdeferrd_r.

    DATA: l_rc               TYPE sy-subrc,
          ls_classkey        TYPE seoclskey.

    ls_classkey-clsname = objname.

    CALL FUNCTION 'SEO_INTDEFERRD_READ_ALL'
      EXPORTING
        cifkey             = ls_classkey
        version            = seoc_version_active
      IMPORTING
        interfacedeferreds = lt_intdeferrds
      EXCEPTIONS
        clif_not_existing  = 1
        OTHERS             = 2.

    LOOP AT lt_intdeferrds INTO ls_intdeferrd.
      lo_intdeferrds = xmldoc->create_element( c_xml_key_intdeferrd ).
      setattributesfromstructure( node      = lo_intdeferrds
                                  structure = ls_intdeferrd ).
      l_rc = xo_rootnode->append_child( lo_intdeferrds ).
    ENDLOOP.

  endmethod.
  method GET_OTR.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
    DATA rootnode TYPE REF TO if_ixml_element.
    DATA txtnode TYPE REF TO if_ixml_element.
    DATA rc TYPE sysubrc.

    DATA sotrheader TYPE sotr_head.
    DATA sotrtextline TYPE sotr_text.
    DATA sotrtexttable TYPE TABLE OF sotr_text.

    DATA _ixml TYPE REF TO if_ixml.
    DATA _xmldoc TYPE REF TO if_ixml_document.

    CALL FUNCTION 'SOTR_GET_CONCEPT'
      EXPORTING
        concept        = otrguid
      IMPORTING
        header         = sotrheader
      TABLES
        entries        = sotrtexttable
      EXCEPTIONS
        no_entry_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

    sotrheader-paket = '$TMP'. "change devclass to $TMP for exports

* Create xml doc
*  _ixml = cl_ixml=>create( ).
*  _xmldoc = _ixml->create_document( ).
*  streamfactory = _ixml->create_stream_factory( ).

* Create parent node
    rootnode = xmldoc->create_element( c_xml_key_sotr ). "OTR object type
    CLEAR sotrheader-concept.                                 "ewH:33
    setattributesfromstructure( node = rootnode structure = sotrheader ).

* Create nodes for texts
    LOOP AT sotrtexttable INTO sotrtextline.
      txtnode = xmldoc->create_element( c_xml_key_sotrtext ).
      CLEAR: sotrtextline-concept, sotrtextline-object.       "ewH:33
      setattributesfromstructure(
        node = txtnode structure = sotrtextline ).
      rc = rootnode->append_child( txtnode ).
    ENDLOOP.

    node = rootnode.

  endmethod.
  method GET_TYPEPUSAGE.
    DATA: lt_typepusages     TYPE seot_typepusages_r,
          lo_typepusages     TYPE REF TO if_ixml_element,
          ls_typepusage      TYPE seot_typepusage_r.

    DATA: l_rc               TYPE sy-subrc,
          l_string           TYPE string,
          ls_classkey        TYPE seoclskey.

    ls_classkey-clsname = objname.

    CALL FUNCTION 'SEO_TYPEPUSAGE_READ_ALL'
      EXPORTING
        cifkey            = ls_classkey
        version           = seoc_version_active
      IMPORTING
        typepusages       = lt_typepusages
      EXCEPTIONS
        clif_not_existing = 1
        OTHERS            = 2.

    LOOP AT lt_typepusages INTO ls_typepusage.
      lo_typepusages = xmldoc->create_element( c_xml_key_typepusage ).
      setattributesfromstructure( node      = lo_typepusages
                                  structure = ls_typepusage ).
      l_rc = xo_rootnode->append_child( lo_typepusages ).
    ENDLOOP.

*ewH: for version 0.1.3, we will continue to generate both nodes
* in order for upgradeability of saplink itself.  For version
* 2.0, forwardDeclaration node generations will be deprecated.
    LOOP AT lt_typepusages INTO ls_typepusage.
      lo_typepusages = xmldoc->create_element( c_xml_key_forwarddeclaration ).
      l_string       = ls_typepusage-typegroup.
      l_rc = lo_typepusages->if_ixml_node~set_value( l_string ).
      l_rc = xo_rootnode->append_child( lo_typepusages ).
    ENDLOOP.

  endmethod.
ENDCLASS.
CLASS ZSAPLINK_CLASS IMPLEMENTATION.
  method CHECKEXISTS.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
  data classkey type SEOCLSKEY.
  data not_active TYPE  SEOX_BOOLEAN.

    classKey-clsName = objname.

    call function 'SEO_CLASS_EXISTENCE_CHECK'
      EXPORTING
        clskey        = classkey
      IMPORTING
        not_active    = not_active
      EXCEPTIONS
*      not_specified = 1
        not_existing  = 2.
*      is_interface  = 3
*      no_text       = 4
*      inconsistent  = 5
*      others        = 6.

    if sy-subrc <> 2.
      exists = 'X'.
    endif.
  endmethod.
  method CREATEIXMLDOCFROMOBJECT.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
    DATA localimplementation TYPE REF TO if_ixml_element.
    DATA localtypes TYPE REF TO if_ixml_element.
    DATA localmacros TYPE REF TO if_ixml_element.
    DATA rootnode TYPE REF TO if_ixml_element.
    DATA reportlist TYPE STANDARD TABLE OF string.
    DATA includename TYPE program.
    DATA _classname TYPE seoclsname.
    DATA reportstring TYPE string.
    DATA rc TYPE sysubrc.
    DATA classdescr TYPE REF TO cl_abap_classdescr.
    DATA typedescr TYPE REF TO cl_abap_typedescr.
    DATA methoddescr TYPE abap_methdescr.
    DATA methodnode TYPE REF TO if_ixml_element.
    DATA parameternode TYPE REF TO if_ixml_element.
    DATA sourcenode TYPE REF TO if_ixml_element.
    DATA exceptionnode TYPE REF TO if_ixml_element.
    DATA exceptionlist TYPE seos_exceptions_r.
    DATA anexception TYPE vseoexcep.
    DATA inheritancenode TYPE REF TO if_ixml_element.
    DATA redefnode TYPE REF TO if_ixml_element.

    DATA tempstring TYPE string.
    DATA methodkey TYPE seocpdkey.
    DATA clsmethkey TYPE seocmpkey.
    DATA methodproperties TYPE vseomethod.
    DATA classkey TYPE seoclskey.
    DATA classproperties TYPE vseoclass.
    DATA paramdescr TYPE abap_parmdescr.
    DATA paramkey TYPE seoscokey.
    DATA paramproperties TYPE vseoparam.
    DATA superclass TYPE REF TO cl_abap_typedescr.
    DATA superclassname TYPE string.
    DATA superclasskey TYPE seorelkey.

    DATA attribdescr TYPE abap_attrdescr.
    DATA attribkey TYPE seocmpkey.
    DATA attribproperties TYPE vseoattrib.
    DATA attribnode TYPE REF TO if_ixml_element.
    DATA inheritanceprops TYPE vseoextend.
    DATA redefines TYPE STANDARD TABLE OF seoredef
        WITH KEY clsname refclsname version mtdname.
    DATA inheritance TYPE seor_inheritance_r.
    DATA redefinitions TYPE seor_redefinitions_r.
    DATA redefinition LIKE LINE OF redefinitions.

    DATA otrnode TYPE REF TO if_ixml_element.
    DATA _otrguid TYPE sotr_conc.

    _classname = objname.
    classkey-clsname = objname.

*  setObjectType( ).

    DATA _objtype TYPE string.
*  _objType = objType.
    _objtype = getobjecttype( ).
    rootnode = xmldoc->create_element( _objtype ).
    CALL FUNCTION 'SEO_CLASS_GET'
      EXPORTING
        clskey       = classkey
        version      = '1'
      IMPORTING
        class        = classproperties
      EXCEPTIONS
        not_existing = 1
        deleted      = 2
        is_interface = 3
        model_only   = 4.

    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 1.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>not_found
              object = objname.
        WHEN 2.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>error_message
              msg    = 'class deleted'.
        WHEN 3.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>error_message
              msg    = 'interfaces not supported'.
        WHEN 4.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>error_message
              msg    = 'class is modeled only'.
      ENDCASE.
    ENDIF.

    setattributesfromstructure( node      = rootnode
                                structure = classproperties ).
    TRY.
        CALL METHOD cl_abap_classdescr=>describe_by_name
          EXPORTING
            p_name         = objname
          RECEIVING
            p_descr_ref    = typedescr
          EXCEPTIONS
            type_not_found = 1.
        IF sy-subrc = 0.
          classdescr ?= typedescr.
        ELSE.

        ENDIF.
      CATCH cx_root.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>system_error.
    ENDTRY.

    CALL METHOD classdescr->get_super_class_type
      RECEIVING
        p_descr_ref           = superclass
      EXCEPTIONS
        super_class_not_found = 1.

    IF sy-subrc = 0.
      superclassname = superclass->get_relative_name( ).
      IF NOT superclassname CS 'OBJECT'.
        superclasskey-clsname = objname.
        superclasskey-refclsname = superclassname.
        CALL FUNCTION 'SEO_INHERITANC_GET'
          EXPORTING
            inhkey        = superclasskey
          IMPORTING
            inheritance   = inheritanceprops
            redefinitions = redefines.
        setattributesfromstructure( node = rootnode structure =
        inheritanceprops ).
      ENDIF.
    ENDIF.

*/***TPJ - Added Logic for TYPES  -------------------*/
    DATA: types      TYPE seoo_types_r,
          wa_type    LIKE LINE OF types,
          types_node TYPE REF TO if_ixml_element.
    CALL FUNCTION 'SEO_TYPE_READ_ALL'
      EXPORTING
        cifkey            = classkey
        version           = 1
      IMPORTING
        types             = types
      EXCEPTIONS
        clif_not_existing = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
    ENDIF.
    LOOP AT types INTO wa_type.
      types_node = xmldoc->create_element( 'types' ).
      setattributesfromstructure( node = types_node structure =
      wa_type ).
      rc = rootnode->append_child( types_node ).
    ENDLOOP.
*/***TPJ - End of Added Logic for TYPES  -------------------*/

*/***TPJ - Added Logic for Friends  -------------------*/
    DATA: clif_keys     TYPE STANDARD TABLE OF seoclskey,
          friends       TYPE STANDARD TABLE OF seofriends,
          wa_friend     LIKE LINE OF friends,
          friends_node  TYPE REF TO if_ixml_element.

    APPEND classkey TO clif_keys.
    CALL FUNCTION 'SEO_FRIENDS_SELECT'
      EXPORTING
        with_external_ref = 'X'
      TABLES
        clif_keys         = clif_keys
        friends_relations = friends.
    IF sy-subrc <> 0.
    ENDIF.
    LOOP AT friends INTO wa_friend.
      friends_node = xmldoc->create_element( c_xml_key_friends ).
      setattributesfromstructure( node = friends_node structure =
      wa_friend ).
      rc = rootnode->append_child( friends_node ).
    ENDLOOP.
*/***TPJ - End of Added Logic for Friends  -------------------*/

*/***ewH - Added Logic for Interfaces  -------------------*/
    DATA: it_implementings TYPE seor_implementings_r,
          wa_implementings LIKE LINE OF it_implementings,
          implementingnode TYPE REF TO if_ixml_element.

    CALL FUNCTION 'SEO_IMPLEMENTG_READ_ALL'
      EXPORTING
        clskey             = classkey
      IMPORTING
        implementings      = it_implementings
      EXCEPTIONS
        class_not_existing = 1
        OTHERS             = 2.

    LOOP AT it_implementings INTO wa_implementings.
      implementingnode = xmldoc->create_element( 'implementing' ).
      setattributesfromstructure( node = implementingnode structure =
      wa_implementings ).
      rc = rootnode->append_child( implementingnode ).
    ENDLOOP.
*/***ewH - End of Added Logic for Interfaces  -------------------*/
*/***rrq - Added Logic for EVENTS  -------------------*/
    DATA: events      TYPE seoo_events_r,
          wa_event    LIKE LINE OF events,
          event_node  TYPE REF TO if_ixml_element,
          eventkey    TYPE seocmpkey,
          eventparams TYPE seos_parameters_r,
          wa_params   TYPE seos_parameter_r.
    CALL FUNCTION 'SEO_EVENT_READ_ALL'
      EXPORTING
        cifkey            = classkey
        version           = 1
      IMPORTING
        events            = events
      EXCEPTIONS
        clif_not_existing = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
    ENDIF.
    LOOP AT events INTO wa_event.
      eventkey-clsname = wa_event-clsname.
      eventkey-cmpname = wa_event-cmpname.
      event_node = xmldoc->create_element( 'events' ).
      setattributesfromstructure( node = event_node structure =
      wa_event ).
      CALL FUNCTION 'SEO_EVENT_SIGNATURE_GET'
        EXPORTING
          evtkey     = eventkey
        IMPORTING
          PARAMETERS = eventparams.

*   parameters
      LOOP AT eventparams INTO wa_params.

        parameternode = xmldoc->create_element( 'parameter' ).
        setattributesfromstructure( node = parameternode
        structure = wa_params ).
        rc = event_node->append_child( parameternode ).
      ENDLOOP.
      rc = rootnode->append_child( event_node ).
    ENDLOOP.
*/***rrq - End of Added Logic for EVENTS  -------------------*/
* removed by Rene.
    get_sections( CHANGING rootnode = rootnode ) .
*|--------------------------------------------------------------------|
    includename = cl_oo_classname_service=>get_ccimp_name( _classname ).
    READ REPORT includename INTO reportlist.
    localimplementation = xmldoc->create_element( 'localImplementation' ).
    reportstring = buildsourcestring( sourcetable = reportlist ).
    rc = localimplementation->if_ixml_node~set_value( reportstring ).
*|--------------------------------------------------------------------|
    includename = cl_oo_classname_service=>get_ccdef_name( _classname ).
    READ REPORT includename INTO reportlist.
    localtypes = xmldoc->create_element( 'localTypes' ).
    reportstring = buildsourcestring( sourcetable = reportlist ).
    rc = localtypes->if_ixml_node~set_value( reportstring ).
*|--------------------------------------------------------------------|
    includename = cl_oo_classname_service=>get_ccmac_name( _classname ).
    READ REPORT includename INTO reportlist.
    localmacros = xmldoc->create_element( 'localMacros' ).
    reportstring = buildsourcestring( sourcetable = reportlist ).
    rc = localmacros->if_ixml_node~set_value( reportstring ).
*|--------------------------------------------------------------------|
*/***EVP - Added Logic for Local Test Classes  ----------------------*/
    DATA localtestclasses TYPE REF TO if_ixml_element.
    DATA localtestclassesexist TYPE i.

    includename = cl_oo_classname_service=>get_local_testclasses_include( _classname ).
    READ REPORT includename INTO reportlist.
    " If sy-subrc = 0 the local test classes do exist
    localtestclassesexist = sy-subrc.
    IF localtestclassesexist = 0.
      localtestclasses = xmldoc->create_element( 'localTestClasses' ).
      reportstring = buildsourcestring( sourcetable = reportlist ).
      rc = localtestclasses->if_ixml_node~set_value( reportstring ).
    ENDIF.
*/***EVP - End of Added Logic for Local Test Classes  ---------------*/
*|                                                                    |
*\--------------------------------------------------------------------/
    rc = rootnode->append_child( localimplementation ).
    rc = rootnode->append_child( localtypes ).
    rc = rootnode->append_child( localmacros ).
*/***EVP - Added Logic for Local Test Classes  -------------------*/
    IF localtestclassesexist = 0.
      rc = rootnode->append_child( localtestclasses ).
    ENDIF.
*/***EVP - End of Added Logic for Local Test Classes  ------------*/
**// Rich:  Start
    get_textpool( CHANGING rootnode = rootnode ).
    get_documentation( CHANGING rootnode = rootnode ).
**// Rich:  End
    get_typepusage( CHANGING  xo_rootnode = rootnode ).
    get_clsdeferrd( CHANGING  xo_rootnode = rootnode ).
    get_intdeferrd( CHANGING  xo_rootnode = rootnode ).

*  classDescriptor ?= cl_abap_typedescr=>describe_by_name( className ).
    attribkey-clsname = objname.

    LOOP AT classdescr->attributes INTO attribdescr
    WHERE is_inherited = abap_false
    AND is_interface = abap_false. "rrq:issue 46
      attribnode = xmldoc->create_element( 'attribute' ).
      attribkey-cmpname = attribdescr-name.
      CALL FUNCTION 'SEO_ATTRIBUTE_GET'
        EXPORTING
          attkey    = attribkey
        IMPORTING
          attribute = attribproperties.

*   include OTR if necessary (for exception classes)
      IF attribproperties-type = 'SOTR_CONC' AND attribproperties-attvalue
      IS NOT INITIAL.
        _otrguid = attribproperties-attvalue+1(32).
        otrnode = get_otr( _otrguid ).
        IF otrnode IS BOUND.
          rc = attribnode->append_child( otrnode ).
        ENDIF.
      ENDIF.

*   append attribute node to parent node
      setattributesfromstructure( node      = attribnode
                                  structure = attribproperties ).
      rc = rootnode->append_child( attribnode ).
    ENDLOOP.

*// ewH: begin of logic for interface methods & inheritance redesign-->
* inheritances & redefinitions: old source removed-recover w/subversion
    CALL FUNCTION 'SEO_INHERITANC_READ'
      EXPORTING
        clskey             = classkey
      IMPORTING
        inheritance        = inheritance
        redefinitions      = redefinitions
      EXCEPTIONS
        class_not_existing = 1
        OTHERS             = 2.

    IF inheritance IS NOT INITIAL.
      inheritancenode = xmldoc->create_element( c_xml_key_inheritance ).
      setattributesfromstructure( node = inheritancenode structure =
      inheritance ).

      LOOP AT redefinitions INTO redefinition.
        redefnode = xmldoc->create_element( 'redefinition' ).
        setattributesfromstructure( node = redefnode structure =
        redefinition ).
        rc = inheritancenode->append_child( redefnode ).
      ENDLOOP.
      rc = rootnode->append_child( inheritancenode ).
    ENDIF.

* methods with out alias We handle this later
    LOOP AT classdescr->methods INTO methoddescr WHERE alias_for IS INITIAL AND
    NOT ( is_inherited = 'X' AND is_redefined IS INITIAL ).
      methodkey-clsname = _classname.
      methodkey-cpdname = methoddescr-name.
*   interface methods
      IF methoddescr-is_interface = 'X'.
        CALL METHOD cl_oo_classname_service=>get_method_include
          EXPORTING
            mtdkey              = methodkey
          RECEIVING
            result              = includename
          EXCEPTIONS
            method_not_existing = 1.
        IF sy-subrc = 0.
          methodnode = xmldoc->create_element( 'interfaceMethod' ).
          setattributesfromstructure( node = methodnode structure =
          methodkey ).
          sourcenode = xmldoc->create_element( 'source' ).
*        tempString = includeName.
*        rc = sourceNode->set_attribute(
*          name = 'includeName' value = tempString ).
          READ REPORT includename INTO reportlist.
          reportstring = buildsourcestring( sourcetable = reportlist ).
          rc = sourcenode->if_ixml_node~set_value( reportstring ).
          rc = methodnode->append_child( sourcenode ).
          rc = rootnode->append_child( methodnode ).
        ENDIF.
*   other methods
      ELSE.
        clsmethkey-clsname = _classname.
        clsmethkey-cmpname = methoddescr-name.
        CLEAR methodproperties.

        IF methoddescr-is_redefined = 'X'.
          methodnode = xmldoc->create_element( 'method' ).
          MOVE-CORRESPONDING clsmethkey TO methodproperties.
*// ewh: begin of forward compatibility hack, can be removed for next
*//      major release-->
          READ TABLE redefinitions INTO redefinition
            WITH KEY mtdname = methoddescr-name.
          IF sy-subrc = 0.
            methodproperties-clsname = redefinition-refclsname.
          ENDIF.
*//<--ewH: end of forward compatibility hack
          setattributesfromstructure( node = methodnode structure =
          methodproperties ).
        ELSE.
          CALL FUNCTION 'SEO_METHOD_GET'
            EXPORTING
              mtdkey       = clsmethkey
            IMPORTING
              method       = methodproperties
            EXCEPTIONS
              not_existing = 1.
          IF sy-subrc = 0.
            methodnode = xmldoc->create_element( 'method' ).
            setattributesfromstructure( node = methodnode structure =
            methodproperties ).

*         parameters
            LOOP AT methoddescr-parameters INTO paramdescr.
              CLEAR paramproperties.
              parameternode = xmldoc->create_element( 'parameter' ).
              paramkey-cmpname = clsmethkey-cmpname.
              paramkey-sconame = paramdescr-name.
              paramkey-clsname = objname.
              CALL FUNCTION 'SEO_PARAMETER_GET'
                EXPORTING
                  parkey    = paramkey
                  version   = '1'
                IMPORTING
                  parameter = paramproperties.
              setattributesfromstructure( node = parameternode
              structure = paramproperties ).
              rc = methodnode->append_child( parameternode ).
            ENDLOOP.

*         exceptions
            CALL FUNCTION 'SEO_METHOD_SIGNATURE_GET'
              EXPORTING
                mtdkey  = clsmethkey
                version = '1'
              IMPORTING
                exceps  = exceptionlist.
            LOOP AT exceptionlist INTO anexception.
              exceptionnode = xmldoc->create_element( 'exception' ).
              setattributesfromstructure( node = exceptionnode
              structure = anexception ).
              rc = methodnode->append_child( exceptionnode ).
            ENDLOOP.
          ENDIF. "method found
        ENDIF. "is_redefined?
*     source
        CALL METHOD cl_oo_classname_service=>get_method_include
          EXPORTING
            mtdkey              = methodkey
          RECEIVING
            result              = includename
          EXCEPTIONS
            method_not_existing = 1.
        IF sy-subrc = 0.
          READ REPORT includename INTO reportlist.
          reportstring = buildsourcestring( sourcetable = reportlist ).
          sourcenode = xmldoc->create_element( 'source' ).
          rc = sourcenode->if_ixml_node~set_value( reportstring ).
          rc = methodnode->append_child( sourcenode ).
        ENDIF.
** StartInsert Rich - Handle method documenation
        get_method_documentation(  EXPORTING method_key = methodkey
                                   CHANGING  rootnode   = methodnode ).
** EndInsert Rich - Handle method documenation
        rc = rootnode->append_child( methodnode ).
      ENDIF. "is_interface?
    ENDLOOP.
* create alias info for load.
    get_alias_method( EXPORTING it_methods     = classdescr->methods
                      CHANGING  xo_rootnode    = rootnode ).
* append root node to xmldoc
    rc = xmldoc->append_child( rootnode ).
    ixmldocument = xmldoc.
*// <--ewH: end of logic for interface methods & inheritance redesign
  endmethod.
  method CREATEOBJECTFROMIXMLDOC.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
    DATA rootnode TYPE REF TO if_ixml_element.
    DATA classkey TYPE seoclskey.
    DATA filter TYPE REF TO if_ixml_node_filter.
    DATA iterator TYPE REF TO if_ixml_node_iterator.
    DATA node TYPE REF TO if_ixml_element.
    DATA otrnode TYPE REF TO if_ixml_element.
    DATA filter2 TYPE REF TO if_ixml_node_filter.
    DATA iterator2 TYPE REF TO if_ixml_node_iterator.
    DATA superclass TYPE vseoextend-clsname.
    DATA superclasskey TYPE vseoextend.
    DATA methodsourcenode TYPE REF TO if_ixml_node.
    DATA sourcenode TYPE REF TO if_ixml_node.
    DATA source TYPE string.
    DATA sourcetable TYPE TABLE OF string.
    DATA methodkey TYPE seocpdkey.
    DATA node2 TYPE REF TO if_ixml_element.
    DATA _objtype TYPE string.
    DATA aobjname TYPE e071-obj_name.
    DATA inheritancenode TYPE REF TO if_ixml_element.
    DATA redefnode TYPE REF TO if_ixml_element.
    DATA includename TYPE program.
    DATA mtdkey   TYPE seocpdkey.

*data excClass type ref to ZCX_SAPLINK.

*// --> begin of new data type rrq
    DATA:
*exporting dataTypes
    e_corrnr                 TYPE trkorr,
    e_devclass               TYPE devclass,
    e_version                TYPE seoversion,
    e_genflag                TYPE genflag,
    e_authority_check        TYPE seox_boolean,
    e_overwrite              TYPE seox_boolean,
*e_suppress_meth_gen      type SEOX_BOOLEAN,
*e_suppress_refac_gen     type SEOX_BOOLEAN,
    e_method_sources         TYPE seo_method_source_table,
    e_locals_def             TYPE rswsourcet,
    e_locals_imp             TYPE rswsourcet,
    e_locals_mac             TYPE rswsourcet,
*e_suppress_ind_update    type SEOX_BOOLEAN,
*importing dataTypes
    i_korrnr                 TYPE trkorr,
*changing dataTypes
    ch_class                 TYPE vseoclass,
    ch_inheritance           TYPE vseoextend,
    ch_redefinitions         TYPE seor_redefinitions_r,
    ch_implementings         TYPE seor_implementings_r,
    ch_impl_details          TYPE seo_redefinitions,
    ch_attributes            TYPE seoo_attributes_r,
    ch_methods               TYPE seoo_methods_r,
    ch_events                TYPE seoo_events_r,
    ch_types                 TYPE seoo_types_r,
    ch_type_source           TYPE seop_source,
    ch_type_source_temp      TYPE seop_source,
    ch_parameters            TYPE seos_parameters_r,
    ch_exceps                TYPE seos_exceptions_r,
    ch_aliases               TYPE seoo_aliases_r,
    ch_typepusages           TYPE seot_typepusages_r,
    ch_clsdeferrds           TYPE seot_clsdeferrds_r,
    ch_intdeferrds           TYPE seot_intdeferrds_r,
    ch_friendships           TYPE seo_friends,
**table dataTypes
*tb_classDescription      type table of seoclasstx,
*tb_component_descr       type table of seocompotx,
*tb_subcomponent_descr    type table of seosubcotx,
* work areas for the tables
    wa_attributes            TYPE seoo_attribute_r,
    wa_types                 TYPE seoo_type_r,
    wa_friends               TYPE seofriends,
    wa_implementings         TYPE seor_implementing_r,
    wa_redefinitions         TYPE seoredef,
    wa_methods               TYPE seoo_method_r,
    wa_parameters            TYPE seos_parameter_r,
    wa_exceps                TYPE seos_exception_r,
    wa_method_sources        TYPE seo_method_source,
    wa_events                TYPE seoo_event_r.
    DATA: lines TYPE i,
          l_msg TYPE string.
*//<-- end of new data types rrq

    CALL FUNCTION 'SEO_BUFFER_INIT'.

    e_devclass = devclass.
    _objtype = getobjecttype( ).
    e_overwrite = overwrite.
    xmldoc = ixmldocument.
    rootnode = xmldoc->find_from_name( _objtype ).

    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = rootnode
      CHANGING
        structure = ch_class.

    objname = classkey-clsname = ch_class-clsname.
    ch_class-version = '0'.
    superclass = rootnode->get_attribute( name = 'REFCLSNAME' ).
    IF superclass IS NOT INITIAL.
* set something for inheritence
      superclasskey-clsname = classkey-clsname.
      superclasskey-refclsname = superclass.
      superclasskey-version = '0'.
      superclasskey-state = '1'.
      MOVE-CORRESPONDING superclasskey TO ch_inheritance.
      ch_inheritance-author = 'BCUSER'.
      ch_inheritance-createdon = sy-datum.
    ENDIF.

*Add attributes to new class
    DATA otrconcept TYPE sotr_text-concept.
    filter = xmldoc->create_filter_name( 'attribute' ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).

    WHILE node IS NOT INITIAL.
*   create OTR texts if necessary (for exception classes)
      CLEAR otrconcept.
      otrnode = node->find_from_name( c_xml_key_sotr ).
      IF otrnode IS NOT INITIAL.
*     ewH:33-->create new concept with new guid
*      me->createotrfromnode( otrnode ).
        me->create_otr(
          EXPORTING node = otrnode
          IMPORTING concept = otrconcept ).
      ENDIF.
      CLEAR wa_attributes.
*   create attribute
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = wa_attributes.
      wa_attributes-version = '0'.
*   ewH:issue33-->6.40 and above, must create new concept
      IF otrconcept IS NOT INITIAL.
        CONCATENATE `'` otrconcept `'` INTO wa_attributes-attvalue.
      ENDIF.
      APPEND wa_attributes TO ch_attributes.
      node ?= iterator->get_next( ).
    ENDWHILE.

*/***TPJ - Added Logic for TYPES  -------------------*/
*  DATA: types           TYPE seoo_types_r,
*        type_properties LIKE LINE OF types.

    filter = xmldoc->create_filter_name( 'types' ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CLEAR wa_types.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = wa_types.
      wa_types-version = '0'.
      APPEND wa_types TO ch_types.
      node ?= iterator->get_next( ).
    ENDWHILE.
*/***TPJ - End of Added Logic for TYPES  -------------------*/

*/***TPJ - Added Logic for Friends  -------------------*/
*  DATA: wa_friends type seofriends.

    filter = xmldoc->create_filter_name( C_XML_KEY_FRIENDS ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CLEAR wa_friends.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = wa_friends.
      wa_friends-version = '0'.
      APPEND wa_friends TO ch_friendships.
      node ?= iterator->get_next( ).
    ENDWHILE.
*/***TPJ - End of Added Logic for Friends  -------------------*/

*// ewH: Added Logic for Implementings(interfaces)-->
    filter = xmldoc->create_filter_name( 'implementing' ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CLEAR wa_implementings.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = wa_implementings.
      APPEND wa_implementings TO ch_implementings.
      node ?= iterator->get_next( ).
    ENDWHILE.
*//<--ewH: End of Added Logic for Implementings(interfaces)

*// rrq: Added Logic for events-->
    filter = xmldoc->create_filter_name( 'events' ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CLEAR wa_events.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = wa_events.
      APPEND wa_events TO ch_events.
      filter2 = node->create_filter_name( 'parameter' ).
      iterator2 = node->create_iterator_filtered( filter2 ).
      node2 ?= iterator2->get_next( ).
      WHILE node2 IS NOT INITIAL.
        CLEAR wa_parameters.
        CALL METHOD getstructurefromattributes
          EXPORTING
            node      = node2
          CHANGING
            structure = wa_parameters.

        "//-> Mar: Added logic for parameter/interface implementation - 08/20/2008
        IF NOT wa_parameters-clsname IS INITIAL.
          APPEND wa_parameters TO ch_parameters.
        ENDIF.
        "//<- Mar: Added logic for parameter/interface implementation - 08/20/2008

        node2 ?= iterator2->get_next( ).
      ENDWHILE.
      node ?= iterator->get_next( ).
    ENDWHILE.
*//<--rrq: End of Added Logic for events

*// ewH: start redesign method/inheritances-->
* inheritance
    inheritancenode = rootnode->find_from_name( c_xml_key_inheritance ).
    IF inheritancenode IS BOUND.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = inheritancenode
        CHANGING
          structure = ch_inheritance.
*   redefs
      filter = inheritancenode->create_filter_name( 'redefinition' ).
      iterator = inheritancenode->create_iterator_filtered( filter ).
      redefnode ?= iterator->get_next( ).
      WHILE redefnode IS NOT INITIAL.
        CALL METHOD getstructurefromattributes
          EXPORTING
            node      = redefnode
          CHANGING
            structure = wa_redefinitions.
        APPEND wa_redefinitions TO ch_redefinitions.
        redefnode ?= iterator->get_next( ).
      ENDWHILE.
    ENDIF.

*Add Methods to new class
    filter = xmldoc->create_filter_name( 'method' ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CLEAR wa_methods.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = wa_methods.

*   only create metadata if method is not a redefinition
      READ TABLE ch_redefinitions INTO wa_redefinitions
      WITH KEY mtdname = wa_methods-cmpname.
      IF sy-subrc = 0.
        node ?= iterator->get_next( ).
        CONTINUE.
      ENDIF.
*// ewh: begin of backward compatibility hack, can be removed for next
*//      major release-->
      IF wa_methods-clsname <> ch_class-clsname.
        MOVE-CORRESPONDING wa_methods TO wa_redefinitions.
        wa_redefinitions-clsname = ch_class-clsname.
        wa_redefinitions-refclsname = wa_methods-clsname.
        wa_redefinitions-version = '0'.
        wa_redefinitions-mtdabstrct = ''.
        wa_redefinitions-mtdname = wa_methods-cmpname.
        APPEND wa_redefinitions TO ch_redefinitions.

        node ?= iterator->get_next( ).
        CONTINUE.
      ENDIF.
*// <--ewH: break in backward compatibility hack - 2Bcontinued below

      filter2 = node->create_filter_name( 'parameter' ).
      iterator2 = node->create_iterator_filtered( filter2 ).
      node2 ?= iterator2->get_next( ).
      WHILE node2 IS NOT INITIAL.
        CLEAR wa_parameters.
        CALL METHOD getstructurefromattributes
          EXPORTING
            node      = node2
          CHANGING
            structure = wa_parameters.

        "//-> Mar: Added logic for parameter/interface implementation - 08/20/2008
        IF NOT wa_parameters-clsname IS INITIAL.
          APPEND wa_parameters TO ch_parameters.
        ENDIF.
        "//<- Mar: Added logic for parameter/interface implementation - 08/20/2008

        node2 ?= iterator2->get_next( ).
      ENDWHILE.
      filter2 = node->create_filter_name( 'exception' ).
      iterator2 = node->create_iterator_filtered( filter2 ).
      node2 ?= iterator2->get_next( ).
      WHILE node2 IS NOT INITIAL.
        CALL METHOD getstructurefromattributes
          EXPORTING
            node      = node2
          CHANGING
            structure = wa_exceps.
        APPEND wa_exceps TO ch_exceps.
        node2 ?= iterator2->get_next( ).
      ENDWHILE.
      APPEND wa_methods TO ch_methods.

** StartInsert Rich - Handle method documenation
      create_method_documentation( node = node ).
** EndInsert Rich - Handle method documenation

      node ?= iterator->get_next( ).
    ENDWHILE.
*// <--ewH: end redesign method/inheritances
*// ewh: continuation of backward compatibility hack-->
*  IF ( ch_redefinitions IS NOT INITIAL OR superclass-clsname
*  IS NOT INITIAL ) and ch_inheritance is initial.
*    CALL FUNCTION 'SEO_INHERITANC_CREATE_F_DATA'
*      EXPORTING
*        save          = ' '
*      CHANGING
*        inheritance   = superclasskey
*        redefinitions = ch_redefinitions.
*  ENDIF.
*// <--ewH: end of backward compatibility hack

    create_typepusage( CHANGING xt_typepusages = ch_typepusages ).
    create_clsdeferrd( CHANGING xt_clsdeferrds = ch_clsdeferrds ).
    create_intdeferrd( CHANGING xt_intdeferrds = ch_intdeferrds ).

*Insert source code into the methods
    filter = xmldoc->create_filter_name( 'method' ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).

    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = wa_methods.
      methodkey-clsname = objname.
      methodkey-cpdname = wa_methods-cmpname.
      aobjname = methodkey.
      methodsourcenode = node->find_from_name( 'source' ).
      IF methodsourcenode IS NOT INITIAL.
        CLEAR wa_method_sources.
        source = methodsourcenode->get_value( ).
        sourcetable = buildtablefromstring( source ).
        READ TABLE ch_redefinitions INTO wa_redefinitions
        WITH KEY mtdname = methodkey-cpdname.
        IF sy-subrc = 0.
          wa_method_sources-redefine = 'X'.
        ENDIF.
        wa_method_sources-cpdname = methodkey-cpdname.
        wa_method_sources-source = sourcetable.
        APPEND wa_method_sources TO e_method_sources.
      ENDIF.
      node ?= iterator->get_next( ).
    ENDWHILE.
*
**// ewH: create interface methods-->
    filter = xmldoc->create_filter_name( 'interfaceMethod' ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).

    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = methodkey.
      aobjname = methodkey.
      methodsourcenode = node->find_from_name( 'source' ).
      IF methodsourcenode IS NOT INITIAL.
        CLEAR wa_method_sources.
        source = methodsourcenode->get_value( ).
        sourcetable = buildtablefromstring( source ).
        wa_method_sources-cpdname = methodkey-cpdname.
        READ TABLE ch_redefinitions INTO wa_redefinitions
        WITH KEY mtdname = methodkey-cpdname.
        IF sy-subrc = 0.
          wa_method_sources-redefine = 'X'.
        ENDIF.
*      wa_method_sources-redefine = wa_methods-redefin.
        wa_method_sources-source = sourcetable.

        APPEND wa_method_sources TO e_method_sources.
      ENDIF.

      node ?= iterator->get_next( ).
    ENDWHILE.
*// <--ewH: end create interface methods

* local implementation
    DATA _classname TYPE seoclsname.
    _classname = objname.
    sourcenode = xmldoc->find_from_name( 'localImplementation' ).
    IF sourcenode IS NOT INITIAL.
      source = sourcenode->get_value( ).
      e_locals_imp = buildtablefromstring( source ).
    ENDIF.

* local types
    sourcenode = xmldoc->find_from_name( 'localTypes' ).
    IF sourcenode IS NOT INITIAL.
      source = sourcenode->get_value( ).
      e_locals_def = buildtablefromstring( source ).
    ENDIF.

* local macros
    sourcenode = xmldoc->find_from_name( 'localMacros' ).
    IF sourcenode IS NOT INITIAL.
      source = sourcenode->get_value( ).
      e_locals_mac = buildtablefromstring( source ).
    ENDIF.
* We don't need the sections for now. Code moved by Rene
    create_sections( ).

*Add Alias to new class
    create_alias_method( CHANGING xt_aliases_method = ch_aliases ).

    name = objname.

    CALL FUNCTION 'SEO_CLASS_CREATE_COMPLETE'
     EXPORTING
       corrnr                             = e_corrnr
       devclass                           = e_devclass
       version                            = e_version
       genflag                            = e_genflag
       authority_check                    = e_authority_check
       overwrite                          = e_overwrite
*   SUPPRESS_METHOD_GENERATION         = e_suppress_meth_gen
*   SUPPRESS_REFACTORING_SUPPORT       = e_suppress_refac_gen
*     method_sources                     = e_method_sources
       locals_def                         = e_locals_def
       locals_imp                         = e_locals_imp
       locals_mac                         = e_locals_mac
*   SUPPRESS_INDEX_UPDATE              = e_suppress_ind_update
     IMPORTING
       korrnr                             = i_korrnr
* TABLES
*   CLASS_DESCRIPTIONS                 = tb_classDescription
*   COMPONENT_DESCRIPTIONS             = tb_component_descr
*   SUBCOMPONENT_DESCRIPTIONS          = tb_subcomponent_descr
      CHANGING
        class                              = ch_class
       inheritance                        = ch_inheritance
       redefinitions                      = ch_redefinitions
       implementings                      = ch_implementings
       impl_details                       = ch_impl_details
       attributes                         = ch_attributes
       methods                            = ch_methods
       events                             = ch_events
       types                              = ch_types
*   TYPE_SOURCE                        = ch_type_source "???
       PARAMETERS                         = ch_parameters
       exceps                             = ch_exceps
       aliases                            = ch_aliases
       typepusages                        = ch_typepusages
       clsdeferrds                        = ch_clsdeferrds
       intdeferrds                        = ch_intdeferrds
       friendships                        = ch_friendships
     EXCEPTIONS
       existing                           = 1
       is_interface                       = 2
       db_error                           = 3
       component_error                    = 4
       no_access                          = 5
       other                              = 6
       OTHERS                             = 7.
    CASE sy-subrc.
      WHEN '0'.
** i guess if we made it this far, we will assume success
** successful install
      WHEN '1'.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>existing.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>system_error.
    ENDCASE.
* Now let's add the methods
    LOOP AT e_method_sources INTO wa_method_sources.
      mtdkey-clsname = objname.
      mtdkey-cpdname = wa_method_sources-cpdname.

      CALL FUNCTION 'SEO_METHOD_GENERATE_INCLUDE'
        EXPORTING
          mtdkey                               = mtdkey
          version                              = e_version
          force                                = e_overwrite
          redefine                             = wa_method_sources-redefine
*     SUPPRESS_CORR                        = SEOX_FALSE
          implementation_expanded              = wa_method_sources-source
*     IMPLEMENTATION                       =
          suppress_mtdkey_check                = seox_true
*     EDITOR_LOCK                          = SEOX_FALSE
*     GENERATED                            = SEOX_FALSE
          corrnr                               = e_corrnr
          without_method_frame                 = seox_true
*     WITH_SUPER_CALL                      = SEOX_FALSE
*     SUPPRESS_INDEX_UPDATE                = SEOX_FALSE
*     EXTEND                               = SEOX_FALSE
*     ENHANCEMENT                          = ' '
*     SUPPRESS_MODIFICATION_SUPPORT        = SEOX_FALSE
     EXCEPTIONS
       not_existing                         = 1
       model_only                           = 2
       include_existing                     = 3
       method_imp_not_generated             = 4
       method_imp_not_initialised           = 5
       _internal_class_not_existing         = 6
       _internal_method_overflow            = 7
       cancelled                            = 8
       method_is_abstract_implemented       = 9
       method_is_final_implemented          = 10
       internal_error_insert_report         = 11
       OTHERS                               = 12
                .
      CASE sy-subrc.
        WHEN '0'.
** i guess if we made it this far, we will assume success
** successful install
        WHEN '3'.
          l_msg = mtdkey.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>existing
              msg    = l_msg.
        WHEN OTHERS.
          l_msg = mtdkey.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>system_error
              msg    = l_msg.
      ENDCASE.
    ENDLOOP.

*ewH:insert pub, prot, and priv sections manually to keep any direct
* attribute/type definitions
    aobjname = classkey-clsname.
**public
    sourcenode = xmldoc->find_from_name( 'publicSection' ).
    IF sourcenode IS NOT INITIAL.
      includename = cl_oo_classname_service=>get_pubsec_name( _classname ).
      source = sourcenode->get_value( ).
      sourcetable = buildtablefromstring( source ).
      INSERT REPORT includename FROM sourcetable EXTENSION TYPE
      srext_ext_class_public STATE 'I'.
    ENDIF.

**protected
    sourcenode = xmldoc->find_from_name( 'protectedSection' ).
    IF sourcenode IS NOT INITIAL.
      includename = cl_oo_classname_service=>get_prosec_name( _classname ).
      source = sourcenode->get_value( ).
      sourcetable = buildtablefromstring( source ).
      INSERT REPORT includename FROM sourcetable EXTENSION TYPE
      srext_ext_class_protected STATE 'I'.
    ENDIF.

**private
    sourcenode = xmldoc->find_from_name( 'privateSection' ).
    IF sourcenode IS NOT INITIAL.
      includename = cl_oo_classname_service=>get_prisec_name( _classname ).
      source = sourcenode->get_value( ).
      sourcetable = buildtablefromstring( source ).
      INSERT REPORT includename FROM sourcetable EXTENSION TYPE
      srext_ext_class_private STATE 'I'.
    ENDIF.
*/***EVP - Added Logic for Local Test Classes  -------------------*/
**local test classes
    sourcenode = xmldoc->find_from_name( 'localTestClasses' ).
    IF sourcenode IS NOT INITIAL.
      DATA clskey TYPE seoclskey.
      source = sourcenode->get_value( ).
      sourcetable = buildtablefromstring( source ).

      clskey-clsname = _classname.
      CALL FUNCTION 'SEO_CLASS_GENERATE_LOCALS'
        EXPORTING
          clskey                 = clskey
          force                  = overwrite
          locals_testclasses     = sourcetable
        EXCEPTIONS
          not_existing           = 1
          model_only             = 2
          locals_not_generated   = 3
          locals_not_initialised = 4
          OTHERS                 = 5.
      IF sy-subrc <> 0.
      ENDIF.
    ENDIF.
*/***EVP - End Of Added Logic for Local Test Classes  -------------------*/

**// Rich:  Start
* Create class textpool
    create_textpool( ).

    create_documentation( ).
**// Rich:  End

* insert inactive sections into worklist
    CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
      EXPORTING
        object            = 'CPUB'
        obj_name          = aobjname
      EXCEPTIONS
        wrong_object_name = 1.
    IF sy-subrc <> 0.
    ENDIF.

    CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
      EXPORTING
        object            = 'CPRO'
        obj_name          = aobjname
      EXCEPTIONS
        wrong_object_name = 1.
    IF sy-subrc <> 0.
    ENDIF.

    CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
      EXPORTING
        object            = 'CPRI'
        obj_name          = aobjname
      EXCEPTIONS
        wrong_object_name = 1.
    IF sy-subrc <> 0.
    ENDIF.


  endmethod.
  method CREATE_DOCUMENTATION.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
    DATA txtline_node     TYPE REF TO if_ixml_element.
    DATA txtline_filter   TYPE REF TO if_ixml_node_filter.
    DATA txtline_iterator TYPE REF TO if_ixml_node_iterator.

    DATA docnode          TYPE REF TO if_ixml_element.

    DATA lang_node        TYPE REF TO if_ixml_element.
    DATA lang_filter      TYPE REF TO if_ixml_node_filter.
    DATA lang_iterator    TYPE REF TO if_ixml_node_iterator.

    DATA obj_name TYPE dokhl-object.
    DATA class_name TYPE string.
    DATA language  TYPE string.
    DATA obj_langu TYPE dokhl-langu.
    DATA lv_str TYPE string.
    DATA rc TYPE sy-subrc.

    DATA lt_lines  TYPE TABLE OF tline.
    FIELD-SYMBOLS: <ls_lines> LIKE LINE OF lt_lines.

    docnode = xmldoc->find_from_name( c_xml_key_class_documentation ).

    IF docnode IS NOT BOUND.
      RETURN.
    ENDIF.

    class_name = docnode->get_attribute( name = c_xml_key_object ).
    obj_name = class_name.

* If no class name, then there was no class documenation, just return.
    IF class_name IS INITIAL.
      RETURN.
    ENDIF.

* Get languages from XML
    FREE: lang_filter, lang_iterator, lang_node.
    lang_filter = docnode->create_filter_name( c_xml_key_language ).
    lang_iterator = docnode->create_iterator_filtered( lang_filter ).
    lang_node ?= lang_iterator->get_next( ).
    WHILE lang_node IS NOT INITIAL.

      REFRESH lt_lines.
      language = lang_node->get_attribute( name = c_xml_key_spras ).
      obj_langu = language.

* Get TextLines from XML
      FREE: txtline_filter, txtline_iterator, txtline_node.
      txtline_filter = lang_node->create_filter_name( c_xml_key_textline ).
      txtline_iterator = lang_node->create_iterator_filtered( txtline_filter ).
      txtline_node ?= txtline_iterator->get_next( ).
      WHILE txtline_node IS NOT INITIAL.
        APPEND INITIAL LINE TO lt_lines ASSIGNING <ls_lines>.
        me->getstructurefromattributes(
                EXPORTING   node      = txtline_node
                CHANGING    structure = <ls_lines> ).
        txtline_node ?= txtline_iterator->get_next( ).
      ENDWHILE.

* Delete any documentation that may currently exist.
      CALL FUNCTION 'DOCU_DEL'
        EXPORTING
          id       = 'CL'
          langu    = obj_langu
          object   = obj_name
          typ      = 'E'
        EXCEPTIONS
          ret_code = 1
          OTHERS   = 2.

* Now update with new documentation text
      CALL FUNCTION 'DOCU_UPD'
        EXPORTING
          id       = 'CL'
          langu    = obj_langu
          object   = obj_name
          typ      = 'E'
        TABLES
          line     = lt_lines
        EXCEPTIONS
          ret_code = 1
          OTHERS   = 2.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg    = `Class Documentation object import failed`.
      ENDIF.

      lang_node ?= lang_iterator->get_next( ).
    ENDWHILE.

  endmethod.
  method CREATE_METHOD_DOCUMENTATION.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
    DATA txtline_node     TYPE REF TO if_ixml_element.
    DATA txtline_filter   TYPE REF TO if_ixml_node_filter.
    DATA txtline_iterator TYPE REF TO if_ixml_node_iterator.

    data: methdocnode     TYPE REF TO if_ixml_element.

    DATA lang_node        TYPE REF TO if_ixml_element.
    DATA lang_filter      TYPE REF TO if_ixml_node_filter.
    DATA lang_iterator    TYPE REF TO if_ixml_node_iterator.

    data obj_name type DOKHL-OBJECT.
    data classmeth_name type string.
    data language  type string.
    data obj_langu type DOKHL-LANGU.
    data lv_str type string.
    data rc type sy-subrc.

    DATA lt_lines  TYPE TABLE OF tline.
    FIELD-SYMBOLS: <ls_lines> LIKE LINE OF lt_lines.

    methdocnode = node->find_from_name( 'methodDocumentation' ).

    if methdocnode is not bound.
      return.
    endif.

    classmeth_name = methdocNode->get_attribute( name = 'OBJECT' ).
    obj_name = classmeth_name.

* If no class method name, then there was no class method documenation, just return.
    if classmeth_name is initial.
      return.
    endif.

* Get languages from XML
    FREE: lang_filter, lang_iterator, lang_node.
    lang_filter = methdocNode->create_filter_name( `language` ).
    lang_iterator = methdocNode->create_iterator_filtered( lang_filter ).
    lang_node ?= lang_iterator->get_next( ).
    WHILE lang_node IS NOT INITIAL.

      refresh lt_lines.
      language = lang_node->get_attribute( name = 'SPRAS' ).
      obj_langu = language.

* Get TextLines from XML
      FREE: txtline_filter, txtline_iterator, txtline_node.
      txtline_filter = lang_node->create_filter_name( `textLine` ).
      txtline_iterator = lang_node->create_iterator_filtered( txtline_filter ).
      txtline_node ?= txtline_iterator->get_next( ).
      WHILE txtline_node IS NOT INITIAL.
        APPEND INITIAL LINE TO lt_lines ASSIGNING <ls_lines>.
        me->getstructurefromattributes(
                EXPORTING   node      = txtline_node
                CHANGING    structure = <ls_lines> ).
        txtline_node ?= txtline_iterator->get_next( ).
      ENDWHILE.

* Delete any documentation that may currently exist.
      CALL FUNCTION 'DOCU_DEL'
        EXPORTING
          id       = 'CO'
          langu    = obj_langu
          object   = obj_name
          typ      = 'E'
        EXCEPTIONS
          ret_code = 1
          OTHERS   = 2.

* Now update with new documentation text
      CALL FUNCTION 'DOCU_UPD'
        EXPORTING
          id       = 'CO'
          langu    = obj_langu
          object   = obj_name
          typ      = 'E'
        TABLES
          line     = lt_lines
        EXCEPTIONS
          ret_code = 1
          OTHERS   = 2.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg    = `Class Method Documentation object import failed`.
      ENDIF.

      lang_node ?= lang_iterator->get_next( ).
    ENDWHILE.

  endmethod.
  method CREATE_SECTIONS.

*ewH-not sure how this type_source param works. type sources can come
* from private or protected sections, but there is no way to pass
* these separately into the class create FM. After debugging into
* FM->clif_save_all->generate_classpool it treats the source table
* as one, so I am not sure how to get it to differentiate between
* private and protected sections. If only one section has types
* defined, the FM call works, otherwise all hell breaks loose. To
* solve the problem for now, we will just do an insert report for
* the sections after the class creation, since that's all the FM
* does in the end anyway. Wow, this is a really long comment, but
* I dont want to have to try to remember what the hell was going
* on here later...sorry.  :)
*insert code for publicSection
*  sourcenode = xmldoc->find_from_name( 'publicSection' )
*  IF sourcenode IS NOT INITIAL.
*    source = sourcenode->get_value( ).
*    ch_type_source = buildtablefromstring( source ).
*  ENDIF.
**insert code for pivateSection
*  sourcenode = xmldoc->find_from_name( 'privateSection' ).
*  IF sourcenode IS NOT INITIAL.
*    source = sourcenode->get_value( ).
*    ch_type_source_temp = buildtablefromstring( source ).
*    append lines of ch_type_source_temp to ch_type_source.
*  ENDIF.
**insert code for ProtectedSection
*  sourcenode = xmldoc->find_from_name( 'protectedSection' ).
*  IF sourcenode IS NOT INITIAL.
*    source = sourcenode->get_value( ).
*    ch_type_source_temp = buildtablefromstring( source ).
*    append lines of ch_type_source_temp to ch_type_source.
*  ENDIF.

  endmethod.
  method CREATE_TEXTPOOL.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
    data textPoolTable type standard table of textPool.
    data textPoolRow type textPool.
    data langIterator type ref to if_ixml_node_iterator.
    data filter type ref to if_ixml_node_filter.
    data textFilter type ref to if_ixml_node_filter.
    data textIterator type ref to if_ixml_node_iterator.
    data textpoolnode type ref to if_ixml_element.
    data langNode type ref to if_ixml_element.
    data aTextNode type ref to if_ixml_element.
    data _objName type TROBJ_NAME.
    data obj_name type SEOCLSNAME.
    data lang type spras.
    data langNodeExists type flag.
    data logonLanguageExists type flag.
    data _state(1) type c.
    data classpoolname type program.

    textpoolnode = xmldoc->find_from_name( 'textPool' ).

    if textpoolnode is not bound.
      return.
    endif.

    obj_name = objName.
    classpoolname = cl_oo_classname_service=>GET_CLASSPOOL_NAME( obj_Name ).
    _objName = classpoolname.

    filter = textPoolNode->create_filter_name( 'language' ).
    langIterator = textPoolNode->create_iterator_filtered( filter ).
    langNode ?= langIterator->get_next( ).


    while langNode is not initial.
      langNodeExists = 'X'.

      CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
        EXPORTING
          OBJECT   = 'REPT'
          OBJ_NAME = _objName
        EXCEPTIONS
          OTHERS   = 0.
      refresh textPoolTable.
      textIterator = langNode->create_iterator( ).
      aTextNode ?= textIterator->get_next( ).
*For some reason the 1st one is blank... not sure why.
      aTextNode ?= textIterator->get_next( ).
      while aTextNode is not initial.
        CALL METHOD GETSTRUCTUREFROMATTRIBUTES
          EXPORTING
            node      = aTextNode
          CHANGING
            structure = textPoolRow.
        append textPoolRow to textPoolTable.
        aTextNode ?= textIterator->get_next( ).
      endwhile.
      if textPoolTable is not initial.
        lang = langNode->get_attribute( 'SPRAS' ).
        if lang = sy-langu.
          logonLanguageExists = 'X'.
          _state = 'I'.
        else.
*       seems that if a textpool is inserted as inactive for language
*       other than the logon language, it is lost upon activation
*       not sure inserting as active is best solution,but seems to work
          _state = 'A'.
        endif.
        insert textpool _objName
          from textPooltable
          language lang
          state    _state.
      endif.
      langNode ?= langIterator->get_next( ).
    endwhile.
  endmethod.
  method DELETEOBJECT.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
  data clsKey type SEOCLSKEY.

    clsKey-clsname = objname.
    CALL FUNCTION 'SEO_CLASS_DELETE_W_DEPS'
      EXPORTING
        clskey             = clsKey
      EXCEPTIONS
       NOT_EXISTING       = 1
       IS_INTERFACE       = 2
       NOT_DELETED        = 3
       DB_ERROR           = 4
       OTHERS             = 5
              .
    if sy-subrc <> 0.
      case sy-subrc.
        when 1.
          raise exception type zcx_saplink
            exporting textid = zcx_saplink=>not_found.
        when 2.
          raise exception type zcx_saplink
            exporting
              textid = zcx_saplink=>error_message
              msg = 'interfaces not supported'.
        when 3.
          raise exception type zcx_saplink
            exporting
              textid = zcx_saplink=>error_message
              msg = 'class not deleted'.
        when others.
          raise exception type zcx_saplink
            exporting textid = zcx_saplink=>system_error.
      endcase.
    endif.
  endmethod.
  method FINDIMPLEMENTINGCLASS.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
  data methodKey type SEOCMPKEY.
  data methodProperties type VSEOMETHOD.
  data classDescr type ref to cl_abap_classdescr.
  data superClass type ref to cl_abap_typeDescr.
  data superClassName type string.

    if startClass is initial.
      methodKey-CLSNAME = objName.
    else.
      methodKey-clsName = startClass.
    endif.
    methodKey-CMPNAME = methodName.

    call function 'SEO_METHOD_GET'
          exporting
            MTDKEY = methodKey
          importing
            method = methodProperties
          exceptions
            NOT_EXISTING = 1.
    if sy-subrc = 0.
      className = methodProperties-clsname.
    else.
      classDescr ?= cl_abap_classDescr=>describe_by_name(
      methodKey-clsName ).
      call method classDescr->GET_SUPER_CLASS_TYPE
          receiving
           P_DESCR_REF = superClass
          exceptions
            SUPER_CLASS_NOT_FOUND = 1.
      superClassName = superClass->GET_RELATIVE_NAME( ).
      className = FINDIMPLEMENTINGCLASS( methodName = methodName
      startClass = superCLassName ).
    endif.
  endmethod.
  method GETOBJECTTYPE.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/

    objecttype = 'CLAS'.  "Class

  endmethod.
  method GET_DOCUMENTATION.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
    DATA languagenode   TYPE REF TO if_ixml_element.
    DATA docnode       TYPE REF TO if_ixml_element.
    DATA txtlines_node TYPE REF TO if_ixml_element.
    DATA rc            TYPE sysubrc.
    DATA _objtype      TYPE string.

    TYPES: BEGIN OF t_dokhl,
            id          TYPE dokhl-id,
            object      TYPE dokhl-object,
            langu       TYPE dokhl-langu,
            typ         TYPE dokhl-typ,
            dokversion  TYPE dokhl-dokversion,
           END OF t_dokhl.

    DATA lt_dokhl TYPE TABLE OF t_dokhl.
    DATA ls_dokhl LIKE LINE OF lt_dokhl.

    DATA lt_lines TYPE TABLE OF tline.
    DATA ls_lines LIKE LINE OF lt_lines.

    DATA lv_str TYPE string.
    DATA _objname TYPE e071-obj_name.

    _objname = objname.

* Check against database
    SELECT  id object langu typ dokversion
          INTO CORRESPONDING FIELDS OF TABLE lt_dokhl
             FROM dokhl
               WHERE id = 'CL'
                  AND object = _objname.

* Use only most recent version.
    SORT lt_dokhl BY id object langu typ ASCENDING dokversion DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_dokhl COMPARING id object typ langu.

* Make sure there is at least one record here.
    CLEAR ls_dokhl.
    READ TABLE lt_dokhl INTO ls_dokhl INDEX 1.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    docnode = xmldoc->create_element( c_xml_key_class_documentation ).

* Set docNode object attribute
    lv_str = ls_dokhl-object.
    rc = docnode->set_attribute( name = c_xml_key_object value = lv_str ).

    LOOP AT lt_dokhl INTO ls_dokhl.

* Create language node, and set attribute
      languagenode = xmldoc->create_element( c_xml_key_language ).
      lv_str = ls_dokhl-langu.
      rc = languagenode->set_attribute( name = c_xml_key_spras value = lv_str ).

* Read the documentation text
      CALL FUNCTION 'DOCU_READ'
        EXPORTING
          id      = ls_dokhl-id
          langu   = ls_dokhl-langu
          object  = ls_dokhl-object
          typ     = ls_dokhl-typ
          version = ls_dokhl-dokversion
        TABLES
          line    = lt_lines.

* Write records to XML node
      LOOP AT lt_lines INTO ls_lines.
        txtlines_node = xmldoc->create_element( c_xml_key_textline ).
        me->setattributesfromstructure( node = txtlines_node structure = ls_lines ).
        rc = languagenode->append_child( txtlines_node ).
      ENDLOOP.
      rc = docnode->append_child( languagenode ) .
    ENDLOOP.

    rc = rootnode->append_child( docnode ).

  endmethod.
  method GET_METHOD_DOCUMENTATION.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
    DATA languagenode   TYPE REF TO if_ixml_element.
    DATA docnode        TYPE REF TO if_ixml_element.
    DATA txtlines_node TYPE REF TO if_ixml_element.
    DATA rc            TYPE sysubrc.
    DATA _objtype      TYPE string.

    TYPES: BEGIN OF t_dokhl,
            id          TYPE dokhl-id,
            object      TYPE dokhl-object,
            langu       TYPE dokhl-langu,
            typ         TYPE dokhl-typ,
            dokversion  TYPE dokhl-dokversion,
           END OF t_dokhl.

    DATA lt_dokhl TYPE TABLE OF t_dokhl.
    DATA ls_dokhl LIKE LINE OF lt_dokhl.

    DATA lt_lines TYPE TABLE OF tline.
    DATA ls_lines LIKE LINE OF lt_lines.

    DATA lv_str TYPE string.
    DATA _objname TYPE e071-obj_name.

    _objname = method_key.

* Check against database
    SELECT  id object langu typ dokversion
          INTO CORRESPONDING FIELDS OF TABLE lt_dokhl
             FROM dokhl
               WHERE id = 'CO'
                  AND object = _objname.

* Use only most recent version.
    SORT lt_dokhl BY id object langu typ ASCENDING dokversion DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_dokhl COMPARING id object typ langu.

* Make sure there is at least one record here.
    CLEAR ls_dokhl.
    READ TABLE lt_dokhl INTO ls_dokhl INDEX 1.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    docnode = xmldoc->create_element( c_xml_key_method_documentation ).

* Set docNode object attribute
    lv_str = ls_dokhl-object.
    rc = docnode->set_attribute( name = c_xml_key_object value = lv_str ).

    LOOP AT lt_dokhl INTO ls_dokhl.

* Create language node, and set attribute
      languagenode = xmldoc->create_element( c_xml_key_language ).
      lv_str = ls_dokhl-langu.
      rc = languagenode->set_attribute( name = c_xml_key_spras value = lv_str ).

* Read the documentation text
      CALL FUNCTION 'DOCU_READ'
        EXPORTING
          id      = ls_dokhl-id
          langu   = ls_dokhl-langu
          object  = ls_dokhl-object
          typ     = ls_dokhl-typ
          version = ls_dokhl-dokversion
        TABLES
          line    = lt_lines.

* Write records to XML node
      LOOP AT lt_lines INTO ls_lines.
        txtlines_node = xmldoc->create_element( c_xml_key_textline ).
        me->setattributesfromstructure( node = txtlines_node structure = ls_lines ).
        rc = languagenode->append_child( txtlines_node ).
      ENDLOOP.
      rc = docnode->append_child( languagenode ) .
    ENDLOOP.

    rc = rootnode->append_child( docnode ).

  endmethod.
  method GET_SECTIONS.
    DATA publicsection TYPE REF TO if_ixml_element.
    DATA protectedsection TYPE REF TO if_ixml_element.
    DATA privatesection TYPE REF TO if_ixml_element.
    DATA includename TYPE program.
    DATA reportstring TYPE string.

**/--------------------------------------------------------------------\
**|                                                                    |
*  includename = cl_oo_classname_service=>get_pubsec_name( _classname ).
*  READ REPORT includename INTO reportlist.
*  publicsection = xmldoc->create_element( 'publicSection' ).
*
*  reportstring = buildsourcestring( sourcetable = reportlist ).
*  rc = publicsection->if_ixml_node~set_value( reportstring ).
*  CLEAR reportstring.
**|--------------------------------------------------------------------|
*  includename = cl_oo_classname_service=>get_prosec_name( _classname ).
*  READ REPORT includename INTO reportlist.
*  protectedsection = xmldoc->create_element( 'protectedSection' ).
*  reportstring = buildsourcestring( sourcetable = reportlist ).
*  rc = protectedsection->if_ixml_node~set_value( reportstring ).
*  CLEAR reportstring.
**|--------------------------------------------------------------------|
*  includename = cl_oo_classname_service=>get_prisec_name( _classname ).
*  READ REPORT includename INTO reportlist.
*  privatesection = xmldoc->create_element( 'privateSection' ).
*  reportstring = buildsourcestring( sourcetable = reportlist ).
*  rc = privatesection->if_ixml_node~set_value( reportstring ).

*  rc = rootnode->append_child( publicsection ).
*  rc = rootnode->append_child( protectedsection ).
*  rc = rootnode->append_child( privatesection ).

  endmethod.
  method GET_TEXTPOOL.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
    DATA atext TYPE REF TO if_ixml_element.
    DATA textpooltable TYPE STANDARD TABLE OF textpool.
    DATA textpoolrow TYPE textpool.
    DATA languagelist TYPE instlang.
    DATA alanguage TYPE spras.
    DATA _objname TYPE seoclsname.
    DATA rc TYPE i.
    DATA stemp TYPE string.
    DATA languagenode TYPE REF TO if_ixml_element.
    DATA textnode      TYPE REF TO if_ixml_element.
    DATA classpoolname TYPE program.
    DATA firstloop TYPE flag.

    _objname = objname.

    classpoolname = cl_oo_classname_service=>get_classpool_name( _objname ).

    CALL FUNCTION 'RS_TEXTLOG_GET_PARAMETERS'
      CHANGING
        installed_languages = languagelist.

    firstloop = abap_true.

    LOOP AT languagelist INTO alanguage.
      READ TEXTPOOL classpoolname INTO textpooltable LANGUAGE alanguage.
      IF sy-subrc = 0.
        IF firstloop = abap_true.
          textnode = xmldoc->create_element( c_xml_key_textpool ).
          firstloop = abap_false.
        ENDIF.
        languagenode = xmldoc->create_element( c_xml_key_language ).
        stemp = alanguage.
        rc = languagenode->set_attribute( name = c_xml_key_spras value = stemp ).
        LOOP AT textpooltable INTO textpoolrow.
          atext = xmldoc->create_element( c_xml_key_textelement ).
          setattributesfromstructure( node = atext structure =
          textpoolrow ).
          rc = languagenode->append_child( atext ).
        ENDLOOP.
        rc = textnode->append_child( languagenode ).
      ENDIF.
    ENDLOOP.

    rc = rootnode->append_child( textnode ).

  endmethod.
ENDCLASS.
CLASS ZSAPLINK_PROGRAM IMPLEMENTATION.
  method CHECKEXISTS.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/

    select single name from trdir into objName where NAME = objName.
    if sy-subrc = 0.
      exists = 'X'.
    endif.

  endmethod.
  method CREATEIXMLDOCFROMOBJECT.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
  data rootNode type ref to if_ixml_element.
  data sourceNode type ref to if_ixml_element.
  data textPoolNode type ref to if_ixml_element.
  data docNode type ref to if_ixml_element.
  data dynproNode type ref to if_ixml_element.
  data statusNode type ref to if_ixml_element.
  data rc type sysubrc.
  data progAttribs type trdir.
  data progSource type RSWSOURCET.
  data sourceString type string.
  data _objType type string.

    _objType = getObjectType( ).
    rootNode = xmlDoc->create_element( _objType ).
    sourceNode = xmlDoc->create_element( 'source' ).
    select single * from trdir into progAttribs where NAME = objName.
    if sy-subrc = 0.
      setAttributesFromStructure( node = rootNode structure =  progAttribs ).
      progSource = me->get_source( ).
      sourceString = buildSourceString( sourceTable = progSource ).
      rc = sourceNode->IF_IXML_NODE~SET_VALUE( sourceString ).
      textPoolNode = get_textPool( ).
      rc = rootNOde->append_child( textPoolNode ).
      docNode = get_documentation( ).
      rc = rootNOde->append_child( docNode ).
      dynproNode = get_dynpro( ).
      rc = rootNode->append_child( dynproNode ).
      statusNode = get_pfstatus( ).
      rc = rootNode->append_child( statusNode ).
      rc = rootNode->append_child( sourceNode ).
      rc = xmldoc->append_child( rootNode ).
      ixmlDocument = xmlDoc.
    else.
      clear ixmlDocument.
      raise exception type zcx_saplink
        exporting
          textid = zcx_saplink=>not_found
          object = objname.
    endif.
  endmethod.
  method CREATEOBJECTFROMIXMLDOC.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
    data rootnode type ref to if_ixml_element.
    data progattribs type trdir.
    data sourcenode type ref to if_ixml_element.
    data textnode type ref to if_ixml_element.
    data docnode type ref to if_ixml_element.
    data dynpnode type ref to if_ixml_element.
    data statnode type ref to if_ixml_element.
    data source type string.
    data sourcetable type table_of_strings.
    data _objname(30) type c.
    data aobjname type trobj_name.
    data _objtype type string.
    data checkexists type flag.

*if sy-uname <> 'USDWM01'.
*    _objType = getObjectType( ).
*    xmlDoc = ixmlDocument.
*    rootNode = xmlDoc->find_from_name( _objType ).
*    call method GETSTRUCTUREFROMATTRIBUTES
*          exporting
*            node = rootNode
*          changing
*            structure = progAttribs.
*    objName = progAttribs-NAME.
*
**   check existing
*    select single name from trdir into objName where NAME = objName.
*    if sy-subrc = 0.
*      raise exception type zcx_saplink
*        exporting textid = zcx_saplink=>existing.
*    endif.
*
*    sourceNode = rootNode->find_from_name( 'source' ).
*    source = sourceNode->get_value( ).
*    sourceTable = BUILDTABLEFROMSTRING( source ).
*    insert report progAttribs-NAME from sourceTable.
*
*    commit work.
*
*    call function 'RS_INSERT_INTO_WORKING_AREA'
*      EXPORTING
*        object            = 'REPS'
*        obj_name          = aobjName
*      EXCEPTIONS
*        wrong_object_name = 1.
*    if sy-subrc <> 0.
*
*    endif.
*
*else.

    _objtype = getobjecttype( ).
    xmldoc = ixmldocument.
    rootnode = xmldoc->find_from_name( _objtype ).
    call method getstructurefromattributes
      exporting
        node      = rootnode
      changing
        structure = progattribs.
    objname = progattribs-name.

*  check if object exists
*  select single name from trdir into objName where NAME = objName.
*  if sy-subrc = 0 and overwrite <> 'X'.
*    raise exception type zcx_saplink
*      exporting textid = zcx_saplink=>existing.
*  endif.

    checkexists = checkexists( ).
    if checkexists is not initial.
      if overwrite is initial.
        raise exception type zcx_saplink
          exporting textid = zcx_saplink=>existing.
      else.
*     delete object for new install
        deleteobject( ).
      endif.
    endif.


    enqueue_abap( ).
    transport_copy( author = progattribs-cnam devclass = devclass ).
    sourcenode = rootnode->find_from_name( 'source' ).
    source = sourcenode->get_value( ).
    sourcetable = buildtablefromstring( source ).
    create_source( source = sourcetable attribs = progattribs ).
    textnode = rootnode->find_from_name( 'textPool' ).
    create_textpool( textnode ).
    docnode = rootnode->find_from_name( 'programDocumentation' ).
    create_documentation( docnode ).
    dynpnode = rootnode->find_from_name( 'dynpros' ).
    create_dynpro( dynpnode ).
    statnode = rootnode->find_from_name( 'pfstatus' ).
    create_pfstatus( statnode ).

    dequeue_abap( ).
    update_wb_tree( ).
*endif.

* successful install
    name = objname.

  endmethod.
  method CREATESTRINGFROMOBJECT.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
    data progSource type RSWSOURCET.
    progsource = me->get_source( ).
    string = buildsourcestring( sourcetable = progsource ).
  endmethod.
  method CREATE_DOCUMENTATION.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
    DATA txtline_node     TYPE REF TO if_ixml_element.
    DATA txtline_filter   TYPE REF TO if_ixml_node_filter.
    DATA txtline_iterator TYPE REF TO if_ixml_node_iterator.

    DATA lang_node     TYPE REF TO if_ixml_element.
    DATA lang_filter   TYPE REF TO if_ixml_node_filter.
    DATA lang_iterator TYPE REF TO if_ixml_node_iterator.

    data obj_name type DOKHL-OBJECT.
    data prog_name type string.
    data language  type string.
    data obj_langu type DOKHL-LANGU.
    data lv_str type string.
    data rc type sy-subrc.

    DATA lt_lines  TYPE TABLE OF tline.
    FIELD-SYMBOLS: <ls_lines> LIKE LINE OF lt_lines.

    if docnode is not bound.
      return.
    endif.

    prog_name = docNode->get_attribute( name = 'OBJECT' ).
    obj_name = prog_name.

* If no prog name, then there was no program documenation, just return.
    if prog_name is initial.
      return.
    endif.

* Get languages from XML
    FREE: lang_filter, lang_iterator, lang_node.
    lang_filter = docNode->create_filter_name( `language` ).
    lang_iterator = docNode->create_iterator_filtered( lang_filter ).
    lang_node ?= lang_iterator->get_next( ).
    WHILE lang_node IS NOT INITIAL.

      refresh lt_lines.
      language = lang_node->get_attribute( name = 'SPRAS' ).
      obj_langu = language.

* Get TextLines from XML
      FREE: txtline_filter, txtline_iterator, txtline_node.
      txtline_filter = lang_node->create_filter_name( `textLine` ).
      txtline_iterator = lang_node->create_iterator_filtered( txtline_filter ).
      txtline_node ?= txtline_iterator->get_next( ).
      WHILE txtline_node IS NOT INITIAL.
        APPEND INITIAL LINE TO lt_lines ASSIGNING <ls_lines>.
        me->getstructurefromattributes(
                EXPORTING   node      = txtline_node
                CHANGING    structure = <ls_lines> ).
        txtline_node ?= txtline_iterator->get_next( ).
      ENDWHILE.

* Delete any documentation that may currently exist.
      CALL FUNCTION 'DOCU_DEL'
        EXPORTING
          id       = 'RE'   "<-- Report/program documentation
          langu    = obj_langu
          object   = obj_name
          typ      = 'E'
        EXCEPTIONS
          ret_code = 1
          OTHERS   = 2.

* Now update with new documentation text
      CALL FUNCTION 'DOCU_UPD'
        EXPORTING
          id       = 'RE'
          langu    = obj_langu
          object   = obj_name
          typ      = 'E'
        TABLES
          line     = lt_lines
        EXCEPTIONS
          ret_code = 1
          OTHERS   = 2.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg    = `Program Documentation object import failed`.
      ENDIF.

      lang_node ?= lang_iterator->get_next( ).
    ENDWHILE.

  endmethod.
  method CREATE_DYNPRO.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
    types: begin of tdyn_head_temp.
           include type d020s.
    types: dtext type d020t-dtxt.
    types: end of tdyn_head_temp.

    data: idyn_fldl type table of d021s,
          idyn_flow type table of d022s,
          idyn_mcod type table of d023s.

    data: xdyn_head type  d020s,
          xdyn_fldl type  d021s,
          xdyn_flow type  d022s,
          xdyn_mcod type  d023s.

    data: xdyn_text_string type string.
    data: xdyn_text        type d020t-dtxt .
    data: xdyn_head_temp   type tdyn_head_temp.

    data _objname type trobj_name.

    data dynpros_node       type ref to if_ixml_element.
    data dynpros_filter     type ref to if_ixml_node_filter.
    data dynpros_iterator   type ref to if_ixml_node_iterator.

    data dynpro_node        type ref to if_ixml_element.
    data dynpro_filter      type ref to if_ixml_node_filter.
    data dynpro_iterator    type ref to if_ixml_node_iterator.

    data dynfldl_node       type ref to if_ixml_element.
    data dynfldl_filter     type ref to if_ixml_node_filter.
    data dynfldl_iterator   type ref to if_ixml_node_iterator.

    data dynmcod_node       type ref to if_ixml_element.
    data dynmcod_filter     type ref to if_ixml_node_filter.
    data dynmcod_iterator   type ref to if_ixml_node_iterator.

    data dynflow_node       type ref to if_ixml_element.

    data xdynpro_flow_source type string.
    data idynpro_flow_source type table_of_strings.

    _objname = objname.

    dynpros_node =  dynp_node.
    check dynpros_node is not initial.

    free: dynpro_filter, dynpro_iterator, dynpro_node.
    dynpro_filter = dynpros_node->create_filter_name( 'dynpro' ).
    dynpro_iterator =
          dynpros_node->create_iterator_filtered( dynpro_filter ).
    dynpro_node ?= dynpro_iterator->get_next( ).

    while dynpro_node is not initial.

      clear:    xdyn_head, xdyn_fldl, xdyn_flow, xdyn_mcod.
      refresh:  idyn_fldl, idyn_flow, idyn_mcod.

* Get the header data for the screen.
      call method getstructurefromattributes
        exporting
          node      = dynpro_node
        changing
          structure = xdyn_head_temp.

      xdyn_head    = xdyn_head_temp.
      xdyn_text    = xdyn_head_temp-dtext.

* Retrieve field list
      free: dynfldl_filter, dynfldl_iterator, dynfldl_node.
      dynfldl_filter = dynpro_node->create_filter_name( 'dynprofield' ).
      dynfldl_iterator =
          dynpro_node->create_iterator_filtered( dynfldl_filter ).
      dynfldl_node ?= dynfldl_iterator->get_next( ).
      while dynfldl_node is not initial.
        call method getstructurefromattributes
          exporting
            node      = dynfldl_node
          changing
            structure = xdyn_fldl.
        append xdyn_fldl to idyn_fldl.
        dynfldl_node ?= dynfldl_iterator->get_next( ).
      endwhile.

* Retrieve matchcode data.
      free: dynmcod_filter, dynmcod_iterator, dynmcod_node.
      dynmcod_filter = dynpro_node->create_filter_name( 'dynprofield' ).
      dynmcod_iterator =
           dynpro_node->create_iterator_filtered( dynmcod_filter ).
      dynmcod_node ?= dynmcod_iterator->get_next( ).
      while dynmcod_node is not initial.
        call method getstructurefromattributes
          exporting
            node      = dynmcod_node
          changing
            structure = xdyn_mcod.
        append xdyn_mcod to idyn_mcod.
        dynmcod_node ?= dynmcod_iterator->get_next( ).
      endwhile.

* retieve flow logic source.
      clear xdynpro_flow_source.  refresh idynpro_flow_source.
      clear xdyn_flow.            refresh idyn_flow.
      free dynflow_node.
      dynflow_node = dynpro_node->find_from_name( 'dynproflowsource' ).
      xdynpro_flow_source  = dynflow_node->get_value( ).
      idynpro_flow_source = buildtablefromstring( xdynpro_flow_source ).
      loop at idynpro_flow_source into xdyn_flow.
        append xdyn_flow  to idyn_flow.
      endloop.

* Build dynpro from data
      call function 'RPY_DYNPRO_INSERT_NATIVE'
        exporting
*       suppress_corr_checks           = ' '
*       CORRNUM                        = ' '
          header                         = xdyn_head
          dynprotext                     = xdyn_text
*       SUPPRESS_EXIST_CHECKS          = ' '
*       USE_CORRNUM_IMMEDIATEDLY       = ' '
*       SUPPRESS_COMMIT_WORK           = ' '
        tables
          fieldlist                      = idyn_fldl
          flowlogic                      = idyn_flow
          params                         = idyn_mcod
       exceptions
          cancelled                      = 1
          already_exists                 = 2
          program_not_exists             = 3
          not_executed                   = 4
          others                         = 5.
      if sy-subrc <> 0.
        raise exception type zcx_saplink
          exporting textid = zcx_saplink=>system_error.
      endif.

      dynpro_node ?= dynpro_iterator->get_next( ).

    endwhile.

  endmethod.
  method CREATE_PFSTATUS.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
    data: ista type table of rsmpe_stat,
          ifun type table of rsmpe_funt,
          imen type table of rsmpe_men,
          imtx type table of rsmpe_mnlt,
          iact type table of rsmpe_act,
          ibut type table of rsmpe_but,
          ipfk type table of rsmpe_pfk,
          iset type table of rsmpe_staf,
          idoc type table of rsmpe_atrt,
          itit type table of rsmpe_titt,
          ibiv type table of rsmpe_buts.

    data: xsta type rsmpe_stat,
          xfun type rsmpe_funt,
          xmen type rsmpe_men,
          xmtx type rsmpe_mnlt,
          xact type rsmpe_act,
          xbut type rsmpe_but,
          xpfk type rsmpe_pfk,
          xset type rsmpe_staf,
          xdoc type rsmpe_atrt,
          xtit type rsmpe_titt,
          xbiv type rsmpe_buts.

    data xtrkey type trkey.
    data xadm   type rsmpe_adm.
    data _program type  trdir-name.
    data _objname type trobj_name.

    data stat_node  type ref to if_ixml_element.
    data node       type ref to if_ixml_element.
    data filter     type ref to if_ixml_node_filter.
    data iterator   type ref to if_ixml_node_iterator.

    _objname = objname.

    stat_node =  pfstat_node.
    check stat_node is not initial.

* read pfstatus_sta node
    free: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_sta' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    while node is not initial.
      call method getstructurefromattributes
        exporting
          node      = node
        changing
          structure = xsta.
      append xsta to ista.
      node ?= iterator->get_next( ).
    endwhile.

* read pfstatus_fun node
    free: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_fun' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    while node is not initial.
      call method getstructurefromattributes
        exporting
          node      = node
        changing
          structure = xfun.
      append xfun to ifun.
      node ?= iterator->get_next( ).
    endwhile.

* read pfstatus_men node
    free: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_men' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    while node is not initial.
      call method getstructurefromattributes
        exporting
          node      = node
        changing
          structure = xmen.
      append xmen to imen.
      node ?= iterator->get_next( ).
    endwhile.

* read pfstatus_mtx node
    free: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_mtx' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    while node is not initial.
      call method getstructurefromattributes
        exporting
          node      = node
        changing
          structure = xmtx.
      append xmtx to imtx.
      node ?= iterator->get_next( ).
    endwhile.

* read pfstatus_act node
    free: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_act' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    while node is not initial.
      call method getstructurefromattributes
        exporting
          node      = node
        changing
          structure = xact.
      append xact to iact.
      node ?= iterator->get_next( ).
    endwhile.

* read pfstatus_but node
    free: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_but' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    while node is not initial.
      call method getstructurefromattributes
        exporting
          node      = node
        changing
          structure = xbut.
      append xbut to ibut.
      node ?= iterator->get_next( ).
    endwhile.

* read pfstatus_pfk node
    free: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_pfk' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    while node is not initial.
      call method getstructurefromattributes
        exporting
          node      = node
        changing
          structure = xpfk.
      append xpfk to ipfk.
      node ?= iterator->get_next( ).
    endwhile.

* read pfstatus_set node
    free: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_set' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    while node is not initial.
      call method getstructurefromattributes
        exporting
          node      = node
        changing
          structure = xset.
      append xset to iset.
      node ?= iterator->get_next( ).
    endwhile.

* read pfstatus_doc node
    free: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_doc' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    while node is not initial.
      call method getstructurefromattributes
        exporting
          node      = node
        changing
          structure = xdoc.
      append xdoc to idoc.
      node ?= iterator->get_next( ).
    endwhile.

* read pfstatus_tit node
    free: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_tit' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    while node is not initial.
      call method getstructurefromattributes
        exporting
          node      = node
        changing
          structure = xtit.
      append xtit to itit.
      node ?= iterator->get_next( ).
    endwhile.

* read pfstatus_biv node
    free: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_biv' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    while node is not initial.
      call method getstructurefromattributes
        exporting
          node      = node
        changing
          structure = xbiv.
      append xbiv to ibiv.
      node ?= iterator->get_next( ).
    endwhile.

* Update the gui status
    _program = _objname.

    xtrkey-obj_type = 'PROG'.
    xtrkey-obj_name = _program.
    xtrkey-sub_type = 'CUAD'.
    xtrkey-sub_name = _program.

    call function 'RS_CUA_INTERNAL_WRITE'
      exporting
        program   = _program
        language  = sy-langu
        tr_key    = xtrkey
        adm       = xadm
        state     = 'I'
      tables
        sta       = ista
        fun       = ifun
        men       = imen
        mtx       = imtx
        act       = iact
        but       = ibut
        pfk       = ipfk
        set       = iset
        doc       = idoc
        tit       = itit
        biv       = ibiv
      exceptions
        not_found = 1
        others    = 2.

    if sy-subrc <> 0.
      raise exception type zcx_saplink
        exporting textid = zcx_saplink=>system_error.
    endif.

  endmethod.
  method CREATE_SOURCE.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/

    data _objName type TROBJ_NAME.
    data progLine type PROGDIR.
    data titleInfo type trdirti.
    data reportLine type string.
    data miniReport type table_of_strings.

    _objName = objName.
    call function 'RS_INSERT_INTO_WORKING_AREA'
          exporting
               OBJECT   = 'REPS'
               OBJ_NAME = _objName
          exceptions
               WRONG_OBJECT_NAME = 1.
     INSERT REPORT _objName FROM source STATE 'I'
       program type attribs-subc.  "added to handle includes, etc.
     MOVE 'I' TO progline-STATE.
     move-corresponding attribs to progline.
     progline-idate = sy-datum.
     progline-itime = sy-uzeit.
     progline-CDAT  = sy-datum.
     progline-UDAT  = sy-datum.
     progline-SDATE = sy-datum.
     modify progdir from progline.
*  Are you kidding me?!?  No idea why you need to do this!!
     CONCATENATE 'REPORT' _objName '.' INTO reportLine SEPARATED BY SPACE.
     append reportline to miniReport.
     INSERT REPORT _objName FROM miniReport STATE 'A'
       program type attribs-subc. "added to handle includes, etc.
     MOVE 'A' TO progline-STATE.
     modify progdir from progline.

  endmethod.
  method CREATE_TEXTPOOL.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
    data textPoolTable type standard table of textPool.
    data textPoolRow type textPool.
    data langIterator type ref to if_ixml_node_iterator.
    data filter type ref to if_ixml_node_filter.
    data textFilter type ref to if_ixml_node_filter.
    data textIterator type ref to if_ixml_node_iterator.
    data langNode type ref to if_ixml_element.
    data aTextNode type ref to if_ixml_element.
    data _objName type TROBJ_NAME.
    data lang type spras.
    data langNodeExists type flag.
    data logonLanguageExists type flag.
    data _state(1) type c.

    _objName = objName.
    CHECK textPoolNode IS NOT INITIAL.

    filter = textPoolNode->create_filter_name( 'language' ).
    langIterator = textPoolNode->create_iterator_filtered( filter ).
    langNode ?= langIterator->get_next( ).

    while langNode is not initial.
      langNodeExists = 'X'.
      CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
           EXPORTING
                OBJECT   = 'REPT'
                OBJ_NAME = _objName
           EXCEPTIONS
                OTHERS   = 0.

      refresh textPoolTable.
      textIterator = langNode->create_iterator( ).
      aTextNode ?= textIterator->get_next( ).
*For some reason the 1st one is blank... not sure why.
      aTextNode ?= textIterator->get_next( ).
      while aTextNode is not initial.
        call method GETSTRUCTUREFROMATTRIBUTES
              exporting
                node = aTextNode
              changing
                structure = textPoolRow.
        append textPoolRow to textPoolTable.
        aTextNode ?= textIterator->get_next( ).
      endwhile.
      if textPoolTable is not initial.
        lang = langNode->get_attribute( 'SPRAS' ).
        if lang = sy-langu.
          logonLanguageExists = 'X'.
          _state = 'I'.
        else.
*       seems that if a textpool is inserted as inactive for language
*       other than the logon language, it is lost upon activation
*       not sure inserting as active is best solution,but seems to work
          _state = 'A'.
        endif.
        insert textpool _objName
          from textPooltable
          language lang
          state    _state.
      endif.
      langNode ?= langIterator->get_next( ).
    endwhile.
  endmethod.
  method DELETEOBJECT.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
  data program type sy-repid.

  program = objName.

  CALL FUNCTION 'RS_DELETE_PROGRAM'
    EXPORTING
*   CORRNUMBER                       =
      program                          = program
*   SUPPRESS_CHECKS                  = ' '
*   SUPPRESS_COMMIT                  = ' '
      SUPPRESS_POPUP                   = 'X'
*   MASS_DELETE_CALL                 = ' '
*   WITH_CUA                         = 'X'
*   WITH_DOCUMENTATION               = 'X'
*   WITH_DYNPRO                      = 'X'
*   WITH_INCLUDES                    = ' '
*   WITH_TEXTPOOL                    = 'X'
*   WITH_VARIANTS                    = 'X'
*   TADIR_DEVCLASS                   =
*   SKIP_PROGRESS_IND                = ' '
*   FORCE_DELETE_USED_INCLUDES       = ' '
* IMPORTING
*   CORRNUMBER                       =
*   PROGRAM                          =
* EXCEPTIONS
*   ENQUEUE_LOCK                     = 1
*   OBJECT_NOT_FOUND                 = 2
*   PERMISSION_FAILURE               = 3
*   REJECT_DELETION                  = 4
*   OTHERS                           = 5
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  endmethod.
  method DEQUEUE_ABAP.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
    call function 'RS_ACCESS_PERMISSION'
         exporting
              global_lock              = 'X'
              mode                     = 'FREE'
              object                   = objName
              object_class             = 'ABAP'
         exceptions
              canceled_in_corr         = 1
              enqueued_by_user         = 3
              enqueue_system_failure   = 4
              locked_by_author         = 5
              illegal_parameter_values = 6
              no_modify_permission     = 7
              no_show_permission       = 8
              permission_failure       = 9.

    if sy-subrc <> 0.
      case sy-subrc.
        when 7 or 8 or 9.
          raise exception type zcx_saplink
            exporting
              textid = zcx_saplink=>not_authorized.
        when 5.
          raise exception type zcx_saplink
            exporting
              textid = zcx_saplink=>error_message
              msg = 'object locked'.
        when others.
          raise exception type zcx_saplink
            exporting
              textid = zcx_saplink=>system_error.
      endcase.
    endif.

  endmethod.
  method ENQUEUE_ABAP.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
    call function 'RS_ACCESS_PERMISSION'
         exporting
*            authority_check          = authority_check
              global_lock              = 'X'
              mode                     = 'INSERT'
*            master_language          = trdir-rload
              object                   = objName
              object_class             = 'ABAP'
*       importing
*            transport_key            = trkey_global
*            new_master_language      = trdir-rload
*            devclass                 = devclass_local
         exceptions
              canceled_in_corr         = 1
              enqueued_by_user         = 3
              enqueue_system_failure   = 4
              locked_by_author         = 5
              illegal_parameter_values = 6
              no_modify_permission     = 7
              no_show_permission       = 8
              permission_failure       = 9.

    if sy-subrc <> 0.
      case sy-subrc.
        when 7 or 8 or 9.
          raise exception type zcx_saplink
            exporting
              textid = zcx_saplink=>not_authorized.
        when 5.
          raise exception type zcx_saplink
            exporting
              textid = zcx_saplink=>error_message
              msg = 'object locked'.
        when others.
          raise exception type zcx_saplink
            exporting
              textid = zcx_saplink=>system_error.
      endcase.
    endif.

  endmethod.
  method GETOBJECTTYPE.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
    objectType = 'PROG'. "ABAP Program
  endmethod.
  method GET_DOCUMENTATION.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
    data languageNode   type ref to if_ixml_element.
    DATA txtlines_node TYPE REF TO if_ixml_element.
    DATA rc            TYPE sysubrc.
    DATA _objtype      TYPE string.

    Types: BEGIN OF t_dokhl,
            id          TYPE dokhl-id,
            object      TYPE dokhl-object,
            langu       type dokhl-langu,
            typ         TYPE dokhl-typ,
            dokversion  TYPE dokhl-dokversion,
           END OF t_dokhl.

    data lt_dokhl type table of t_dokhl.
    data ls_dokhl like line of lt_dokhl.

    DATA lt_lines TYPE TABLE OF tline.
    DATA ls_lines LIKE LINE OF lt_lines.

    data lv_str type string.
    DATA _objname TYPE e071-obj_name.

    _objname = objname.

* Check against database
    SELECT  id object langu typ dokversion
          INTO corresponding fields of table lt_dokhl
             FROM dokhl
               WHERE id = 'RE'
                  AND object = _objname.

* Use only most recent version.
    sort lt_dokhl by id object langu typ ascending dokversion descending.
    delete adjacent duplicates from lt_dokhl comparing id object typ langu.

* Make sure there is at least one record here.
    clear ls_dokhl.
    read table lt_dokhl into ls_dokhl index 1.
    if sy-subrc <> 0.
      return.
    endif.

    docNode = xmlDoc->create_element( 'programDocumentation' ).

* Set docNode object attribute
    lv_str = ls_dokhl-object.
    rc = docNode->set_attribute( name = 'OBJECT' value = lv_Str ).

    Loop at lt_dokhl into ls_dokhl.

* Create language node, and set attribute
      languageNode = xmlDoc->create_element( 'language' ).
      lv_str = ls_dokhl-langu.
      rc = languageNode->set_attribute( name = 'SPRAS' value = lv_Str ).

* Read the documentation text
      CALL FUNCTION 'DOCU_READ'
        EXPORTING
          id      = ls_dokhl-id
          langu   = ls_dokhl-langu
          object  = ls_dokhl-object
          typ     = ls_dokhl-typ
          version = ls_dokhl-dokversion
        TABLES
          line    = lt_lines.

* Write records to XML node
      LOOP AT lt_lines INTO ls_lines.
        txtlines_node = xmlDoc->create_element( `textLine` ).
        me->setattributesfromstructure( node = txtlines_node structure = ls_lines ).
        rc = languageNode->append_child( txtlines_node ).
      ENDLOOP.
      rc = docNode->append_child( languageNode ) .
    Endloop.

  endmethod.
  method GET_DYNPRO.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
    types: begin of tdynp,
           prog type d020s-prog,
           dnum type d020s-dnum,
           end of tdynp.

    data: idyn_fldl type table of d021s,
          idyn_flow type table of d022s,
          idyn_mcod type table of d023s.

    data: xdyn_head type  d020s,
          xdyn_fldl type  d021s,
          xdyn_flow type  d022s,
          xdyn_mcod type  d023s.

    data idynp type table of tdynp.
    data xdynp type tdynp.

    data xdyn_text type d020t-dtxt.
    data xdyn_text_string type string.

    data _objname type trobj_name.
    data rc type sy-subrc .

    data iflowsource type rswsourcet.
    data xflowsource like line of iflowsource.
    data flowsourcestring type string.

    data dynnr_node type ref to if_ixml_element.
    data dynpromatchnode type ref to if_ixml_element.
    data dynprofieldsnode type ref to if_ixml_element.
    data dynproflownode type ref to if_ixml_element.

    _objname = objname.

* Get all dynpros for program object
    clear xdynp.  refresh idynp.
    select prog dnum into table idynp
                  from d020s
                     where prog = _objname
                       and type <> 'S'    " No Selection Screens
                       and type <> 'J'.   " No selection subscreens
    check sy-subrc  = 0 .

    dynp_node = xmldoc->create_element( 'dynpros' ).

    loop at idynp into xdynp.

* Retrieve dynpro imformation
      dynnr_node =  xmldoc->create_element( 'dynpro' ).

      clear:    xdyn_head, xdyn_fldl, xdyn_flow, xdyn_mcod.
      refresh:  idyn_fldl, idyn_flow, idyn_mcod.

      call function 'RPY_DYNPRO_READ_NATIVE'
        exporting
          progname                    = xdynp-prog
          dynnr                       = xdynp-dnum
*       SUPPRESS_EXIST_CHECKS       = ' '
*       SUPPRESS_CORR_CHECKS        = ' '
      importing
          HEADER                      = xdyn_head
          dynprotext                  = xdyn_text
       tables
          fieldlist                   = idyn_fldl
          flowlogic                   = idyn_flow
          params                      = idyn_mcod
*       FIELDTEXTS                  =
       exceptions
          cancelled                   = 1
          not_found                   = 2
          permission_error            = 3
          others                      = 4.

      check sy-subrc = 0.

* Add heading information for screen.
      setattributesfromstructure(
                       node = dynnr_node structure =  xdyn_head  ).
* Add the dynpro text also.
      xdyn_text_string =  xdyn_text.
      rc = dynnr_node->set_attribute(
                 name = 'DTEXT'  value = xdyn_text_string ).
      rc = dynp_node->append_child( dynnr_node ).

* Add fields information for screen.
      if not idyn_fldl[] is initial.
        loop at idyn_fldl into xdyn_fldl.
          dynprofieldsnode = xmldoc->create_element( 'dynprofield' ).
          setattributesfromstructure(
                   node = dynprofieldsnode structure =  xdyn_fldl ).
          rc = dynnr_node->append_child( dynprofieldsnode ).
        endloop.
      endif.

* Add flow logic of screen
      if not idyn_flow[] is initial.
        clear xflowsource. refresh  iflowsource.
        loop at idyn_flow into xdyn_flow.
          xflowsource  = xdyn_flow.
          append xflowsource to iflowsource.
        endloop.

        dynproflownode = xmldoc->create_element( 'dynproflowsource' ).
        flowsourcestring = buildsourcestring( sourcetable = iflowsource ).
        rc = dynproflownode->if_ixml_node~set_value( flowsourcestring ).
        rc = dynnr_node->append_child( dynproflownode  ).
      endif.

* Add matchcode information for screen.
      if not idyn_mcod[] is initial.
        loop at idyn_mcod into xdyn_mcod.
          check not xdyn_mcod-type is initial
            and not xdyn_mcod-content is initial.
          dynpromatchnode = xmldoc->create_element( 'dynpromatchcode' ).
          setattributesfromstructure(
                   node = dynpromatchnode structure =  xdyn_mcod ).
          rc = dynnr_node->append_child( dynpromatchnode ).
        endloop.
      endif.

    endloop.

  endmethod.
  method GET_PFSTATUS.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
    data: ista type table of rsmpe_stat,
          ifun type table of rsmpe_funt,
          imen type table of rsmpe_men,
          imtx type table of rsmpe_mnlt,
          iact type table of rsmpe_act,
          ibut type table of rsmpe_but,
          ipfk type table of rsmpe_pfk,
          iset type table of rsmpe_staf,
          idoc type table of rsmpe_atrt,
          itit type table of rsmpe_titt,
          ibiv type table of rsmpe_buts.

    data: xsta type rsmpe_stat,
          xfun type rsmpe_funt,
          xmen type rsmpe_men,
          xmtx type rsmpe_mnlt,
          xact type rsmpe_act,
          xbut type rsmpe_but,
          xpfk type rsmpe_pfk,
          xset type rsmpe_staf,
          xdoc type rsmpe_atrt,
          xtit type rsmpe_titt,
          xbiv type rsmpe_buts.

    data sta_node type ref to if_ixml_element.
    data fun_node type ref to if_ixml_element.
    data men_node type ref to if_ixml_element.
    data mtx_node type ref to if_ixml_element.
    data act_node type ref to if_ixml_element.
    data but_node type ref to if_ixml_element.
    data pfk_node type ref to if_ixml_element.
    data set_node type ref to if_ixml_element.
    data doc_node type ref to if_ixml_element.
    data tit_node type ref to if_ixml_element.
    data biv_node type ref to if_ixml_element.

    data _objname type trobj_name.
    data _program type  trdir-name.
    data rc type sy-subrc.

    _objname = objname.
    _program = objname.

    call function 'RS_CUA_INTERNAL_FETCH'
      exporting
        program         = _program
        language        = sy-langu
      tables
        sta             = ista
        fun             = ifun
        men             = imen
        mtx             = imtx
        act             = iact
        but             = ibut
        pfk             = ipfk
        set             = iset
        doc             = idoc
        tit             = itit
        biv             = ibiv
      exceptions
        not_found       = 1
        unknown_version = 2
        others          = 3.

    check sy-subrc = 0.

* if there is a gui status or gui title present, then
* create pfstatus node.
    if ista[] is not initial
       or itit[] is not initial.
      pfstat_node = xmldoc->create_element( 'pfstatus' ).
    endif.


* if ista is filled, assume there are one or more
* gui statuses
    if ista[] is not initial.

      loop at ista into xsta.
        sta_node = xmldoc->create_element( 'pfstatus_sta' ).
        setattributesfromstructure(
                 node = sta_node
                 structure =  xsta ).
        rc = pfstat_node->append_child( sta_node ).
      endloop.

      loop at ifun into xfun.
        fun_node = xmldoc->create_element( 'pfstatus_fun' ).
        setattributesfromstructure(
                 node = fun_node
                 structure =  xfun ).
        rc = pfstat_node->append_child( fun_node ).
      endloop.

      loop at imen into xmen.
        men_node = xmldoc->create_element( 'pfstatus_men' ).
        setattributesfromstructure(
                 node = men_node
                 structure =  xmen ).
        rc = pfstat_node->append_child( men_node ).
      endloop.

      loop at imtx into xmtx.
        mtx_node = xmldoc->create_element( 'pfstatus_mtx' ).
        setattributesfromstructure(
                 node = mtx_node
                 structure =  xmtx ).
        rc = pfstat_node->append_child( mtx_node ).
      endloop.

      loop at iact into xact.
        act_node = xmldoc->create_element( 'pfstatus_act' ).
        setattributesfromstructure(
                 node = act_node
                 structure =  xact ).
        rc = pfstat_node->append_child( act_node ).
      endloop.

      loop at ibut into xbut.
        but_node = xmldoc->create_element( 'pfstatus_but' ).
        setattributesfromstructure(
                 node = but_node
                 structure =  xbut ).
        rc = pfstat_node->append_child( but_node ).
      endloop.

      loop at ipfk into xpfk.
        pfk_node = xmldoc->create_element( 'pfstatus_pfk' ).
        setattributesfromstructure(
                 node = pfk_node
                 structure =  xpfk ).
        rc = pfstat_node->append_child( pfk_node ).
      endloop.

      loop at iset into xset.
        set_node = xmldoc->create_element( 'pfstatus_set' ).
        setattributesfromstructure(
                 node = set_node
                 structure =  xset ).
        rc = pfstat_node->append_child( set_node ).
      endloop.

      loop at idoc into xdoc.
        doc_node = xmldoc->create_element( 'pfstatus_doc' ).
        setattributesfromstructure(
                 node = doc_node
                 structure =  xdoc ).
        rc = pfstat_node->append_child( doc_node ).
      endloop.


      loop at ibiv into xbiv.
        biv_node = xmldoc->create_element( 'pfstatus_biv' ).
        setattributesfromstructure(
                 node = biv_node
                 structure =  xbiv ).
        rc = pfstat_node->append_child( biv_node ).
      endloop.

    endif.


* It itit is filled, assume one or more titles
    if itit[] is not initial.

      loop at itit into xtit.
        tit_node = xmldoc->create_element( 'pfstatus_tit' ).
        setattributesfromstructure(
                 node = tit_node
                 structure =  xtit ).
        rc = pfstat_node->append_child( tit_node ).
      endloop.

    endif.

  endmethod.
  method GET_SOURCE.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/

    data _objName(30) type c.

    _objName = me->objName.
    read report _objName into progSource.

  endmethod.
  method GET_TEXTPOOL.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
  data aText type ref to if_ixml_element.
  data textPoolTable type standard table of TEXTPOOL.
  data textPoolRow type textPool.
  data languageList type instLang.
  data aLanguage type SPRAS.
  data _objName(30) type c.
  data rc type i.
  data sTemp type string.
  data languageNode type ref to if_ixml_element.
  data firstLoop type flag.

    _objName = objName.


    CALL FUNCTION 'RS_TEXTLOG_GET_PARAMETERS'
          changing
            INSTALLED_LANGUAGES = languageList.

    firstLoop = abap_true.

    loop at languageList into aLanguage.
      read textpool _objName into textPoolTable language aLanguage.
      if sy-subrc = 0.
        if firstLoop = abap_true.
          textNode = xmlDoc->create_element( 'textPool' ).
          firstLoop = abap_false.
        endif.
        languageNode = xmlDoc->create_Element( 'language' ).
        sTemp = aLanguage.
        rc = languageNode->set_attribute( name = 'SPRAS' value = sTemp ).
        loop at textPoolTable into textPoolRow.
          aText = xmlDoc->create_element( 'textElement' ).
          setAttributesFromStructure( node = aText structure =
          textPoolRow ).
          rc = languageNode->append_child( aText ).
        endloop.
        rc = textNode->append_child( languageNode ).
      endif.
    endloop.

  endmethod.
  method TRANSPORT_COPY.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
    CALL FUNCTION 'RS_CORR_INSERT'
         EXPORTING
              AUTHOR              = author
              GLOBAL_LOCK         = 'X'
              OBJECT              = objName
              OBJECT_CLASS        = 'ABAP'
              DEVCLASS            = devClass
*            KORRNUM             = CORRNUMBER_LOCAL
              MASTER_LANGUAGE     = sy-langu
*            PROGRAM             = PROGRAM_LOCAL
              MODE                = 'INSERT'
*       IMPORTING
*            AUTHOR              = UNAME
*            KORRNUM             = CORRNUMBER_LOCAL
*            DEVCLASS            = DEVCLASS_LOCAL
         EXCEPTIONS
              CANCELLED           = 1
              PERMISSION_FAILURE  = 2
              UNKNOWN_OBJECTCLASS = 3.

    if sy-subrc <> 0.
      case sy-subrc.
        when 2.
          raise exception type zcx_saplink
            exporting
              textid = zcx_saplink=>not_authorized.
        when others.
          raise exception type zcx_saplink
            exporting
              textid = zcx_saplink=>system_error.
      endcase.
    endif.

  endmethod.
  method UPDATE_WB_TREE.

    DATA: BEGIN OF pname,
            root(3) VALUE 'PG_',
            program(27),
          END OF pname.

    DATA: trdir TYPE trdir.

    pname-program = me->objname.

    CALL FUNCTION 'WB_TREE_ACTUALIZE'
      EXPORTING
        tree_name = pname.

    trdir-name    = me->objname.

    CALL FUNCTION 'RS_TREE_OBJECT_PLACEMENT'
      EXPORTING
        object    = trdir-name
        program   = trdir-name
        operation = 'INSERT'
        type      = 'CP'.

  endmethod.
ENDCLASS.

type-pools: seor, abap, icon.
data retFileTable type FILETABLE.
data retRc type sysubrc.
data retUserAction type i.

data tempXMLString type string.
data ixmlNugget type ref to if_ixml_document.

data pluginExists type flag.
data objectExists type flag.
data flag type flag.
data statusMsg type string.
DATA  y2all type flag.
selection-screen begin of Line.
  SELECTION-SCREEN COMMENT 1(20) fileCom FOR FIELD NUGGFIL.
  parameters NUGGFIL(300) type c modif id did obligatory.
selection-screen end of Line.

selection-screen begin of Line.
  SELECTION-SCREEN COMMENT 1(20) checkCom FOR FIELD NUGGFIL.
  parameters overwrt type c as checkbox default 'X'.
selection-screen end of Line.



start-of-selection.
clear tempXMLString.
perform uploadXMLFromLM using NUGGFIL tempXMLString.
perform CONVERTSTRINGTOIXMLDOC using tempXMLString changing ixmlNugget.
perform installNugget using ixmlNugget overwrt.


*/--------------------------------------------------------------------\
*| Selection screen events                                            |
initialization.
  fileCom = 'Installation Nugget'.
  checkCom = 'Overwrite Originals'.


at selection-screen on value-request for NUGGFIL.
  call method CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
      exporting
        MULTISELECTION = abap_false
        FILE_FILTER = '*.nugg'
        DEFAULT_EXTENSION = 'nugg'
      changing
        FILE_TABLE = retFileTable
        rc = retRc
        user_Action = retUserAction.
  read table retFileTable into NUGGFIL index 1.
  refresh retFileTable.

*\--------------------------------------------------------------------/


*/--------------------------------------------------------------------\
*| Forms from the SAPLink Installer                                   |
*|                                                                     |
form uploadXMLFromLM using p_filename xmlString type string .
  DATA retfiletable TYPE filetable.
  DATA retrc TYPE sysubrc.
  DATA retuseraction TYPE i.
  DATA temptable TYPE table_of_strings.
  DATA temptable_bin TYPE TABLE OF x255.
  DATA filelength TYPE i.
  DATA l_filename TYPE string.

  l_filename = p_filename.
  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename                = l_filename
      filetype                = 'BIN'       " File Type Binary
    IMPORTING
      filelength              = filelength
    CHANGING
      data_tab                = temptable_bin
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      not_supported_by_gui    = 17
      error_no_gui            = 18
      OTHERS                  = 19.
  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN '1'.
        PERFORM writemessage USING 'E' 'File Open Error'.
      WHEN OTHERS.
        PERFORM writemessage USING 'E' 'Unknown Error occured'.
    ENDCASE.
  ENDIF.

  CALL FUNCTION 'SCMS_BINARY_TO_STRING'
    EXPORTING
      input_length = filelength
    IMPORTING
      text_buffer  = xmlstring
    TABLES
      binary_tab   = temptable_bin.
  IF sy-subrc <> 0.
    " Just catch the sy-subrc when there was nothing replaced
    sy-subrc = 0.
  ENDIF.
*  call method CL_GUI_FRONTEND_SERVICES=>GUI_UPLOAD
*        exporting
*          FILENAME = l_fileName
*        changing
*          data_tab = tempTable.
*  PERFORM createstring USING temptable CHANGING xmlstring.
endform.
*\--------------------------------------------------------------------/
form createString
      using
        tempTable type table_of_strings
      changing
        bigString type string.

data sTemp type string.
  loop at tempTable into sTemp.
    concatenate bigString sTemp CL_ABAP_CHAR_UTILITIES=>NEWLINE into
    bigString.
  endloop.

endform.
*/----------------------------------------------------------------------



*/--------------------------------------------------------------------\
*| Forms from the SAPLink Root Class                                  |
form CONVERTSTRINGTOIXMLDOC
      using
        i_xmlString type string
      changing
        ixmlDocument type ref to if_ixml_document.

  data xmlString type string.
  data ixml type ref to if_ixml.
  data streamFactory type ref to IF_IXML_STREAM_FACTORY.
  data iStream type ref to if_ixml_istream.
  data ixmlParser type ref to if_ixml_parser.
  data xmlDoc type ref to if_ixml_document.

  xmlString = i_xmlString.
  " Make sure to convert Windows Line Break to Unix as
  " this linebreak is used to get a correct import
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf
    IN xmlString WITH cl_abap_char_utilities=>newline.

  ixml = cl_ixml=>create( ).
  xmlDoc = ixml->create_document( ).
  streamFactory = ixml->CREATE_STREAM_FACTORY( ).
  iStream = streamFactory->CREATE_ISTREAM_STRING( xmlString ).
  iXMLParser = iXML->create_parser(  stream_factory = streamFactory
                                     istream        = iStream
                                     document       = xmlDoc ).
  iXMLParser->parse( ).
  ixmlDocument = xmlDoc.

endform.

*|                                                                     |
*|                                                                     |

FORM GETOBJECTInfoFROMIXMLDOC
      using ixmlDocument type ref to if_ixml_document
      changing objTypeName type string objName type string.
data rootNode type ref to IF_IXML_NODE.
data rootAttr type ref to IF_IXML_NAMED_NODE_MAP.
data AttrNode type ref to IF_IXML_NODE.
data nodeName type string.

  rootNode ?= ixmlDocument->GET_ROOT_ELEMENT( ).

* get object type
  objTypeName = rootNode->GET_NAME( ).
  translate objTypeName to upper case.

* get object name
  rootAttr = rootNode->GET_ATTRIBUTES( ).
  AttrNode = rootAttr->GET_ITEM( 0 ).
  objName = AttrNode->GET_VALUE( ).

ENDFORM.

*/--------------------------------------------------------------------\
*|  Nugget Class                                                      |
form     installNugget
      using xmlDoc type ref to if_ixml_document overwrite type c.
types: begin of t_objectTable,
         classname type string,
         object type ko100-object,
         text type ko100-text,
       end of t_objectTable.


data iterator type ref to IF_IXML_NODE_ITERATOR.
data ixml type ref to if_ixml.
data Namefilter type ref to IF_IXML_NODE_FILTER.
data parentFilter type ref to IF_IXML_NODE_FILTER.
data currentNode type ref to if_ixml_node.
data newNode type ref to if_ixml_node.
data rval type i.
data ixmlDocument type ref to if_ixml_document.
data _objName type string.
data objType type string.
data objectTable type table of t_objectTable.
data objectLine type t_objectTable.
data exists type flag.
data sTemp type string.
data nameCollision type flag.
data l_targetObject type ref to zsaplink.
data l_installObject type string.
data l_excClass type ref to ZCX_SAPLINK.
data tempcname type string.

  ixml = cl_ixml=>create( ).
  nameFilter = xmlDoc->create_filter_name( name = 'nugget' ).
  parentFilter = xmlDoc->create_filter_parent( nameFilter ).
  iterator = xmlDoc->create_iterator_filtered( parentFilter ).

  currentNode ?= iterator->get_next( ).
  while currentNode is not initial.
    clear exists.
    ixmlDocument = ixml->create_document( ).
    newNode = currentNode->clone( ).
    rval = ixmlDocument->append_child( newNode ).

    call method zsaplink=>GETOBJECTInfoFROMIXMLDOC
      exporting
        ixmlDocument = ixmlDocument
      importing
        objtypename = objType
        objname     = _objName.

*  call method zsaplink=>getplugins( changing objectTable = objectTable )
*.
*
*  read table objectTable into objectLine with key object = objType.
*
*  if sy-subrc = 0.

    translate objType to upper case.
    case objtype.
      when 'CLAS'.
        tempcname = 'ZSAPLINK_CLASS'.
      when 'PROG'.
        tempcname = 'ZSAPLINK_PROGRAM'.
      when others.
    endcase.

    create object l_targetObject type (tempcname)
      exporting name = _objName.

    objectExists = l_targetObject->checkexists( ).

    if objectExists = 'X' and overWrt = ''.
      write :/  objType, _objName,
      ' exists on this system , if you wish to install this Nugget '
      & 'please set the Overwrite Originals checkbox.'
          .
    elseif objectExists = 'X' and overWrt = 'X'.

      if l_targetObject is not initial.

      if y2all <> 'X'.
        concatenate objType _objName into sTemp separated by space.
        perform confirmOverwrite using sTemp
                              changing flag.
        if flag = '1'. "yes
        elseif flag = '2'. "yes to all
          y2all = 'X'.
        elseif flag = 'A'. "cancel
          write / 'Import cancelled by user'.
*          Flag = 'X'.
          exit.
        endif.
       endif.
        try.
          l_installObject = l_targetObject->createObjectfromiXMLDoc(
                                          ixmlDocument = ixmlDocument
                                          overwrite = overWrt ).

          catch ZCX_SAPLINK into l_excClass.
            statusMsg = l_excClass->get_text( ).
            Flag = 'X'.
        endtry.
        if l_installObject is not initial.
          concatenate 'Installed: ' objType l_installObject
           into statusMsg separated by space.
        endif.
      else.
        statusMsg = 'an undetermined error occured'.
        Flag = 'X'.
      endif.

    else.
        try.
          l_installObject = l_targetObject->createObjectfromiXMLDoc(
                                          ixmlDocument = ixmlDocument
                                          overwrite = overWrt ).

          catch ZCX_SAPLINK into l_excClass.
            statusMsg = l_excClass->get_text( ).
            Flag = 'X'.
        endtry.
        if l_installObject is not initial.
          concatenate 'Installed: ' objType l_installObject
           into statusMsg separated by space.
        endif.
    endif.
  currentNode ?= iterator->get_next( ).
  write: / Statusmsg.
endwhile.
endform.

*/----------------------confirmOverwrite------------------------------\
form confirmOverwrite using l_objInfo type string
                   changing l_answer type flag.

data l_message type string.
data l_title type string.

  clear l_answer.
  l_title = 'Overwrite confirm. Proceed with CAUTION!'.

  concatenate 'You have selected to overwrite originals.'
    l_objinfo 'will be overwritten. Are you sure?'
    into l_message separated by space.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR                    = l_title
      text_question               = l_message
      TEXT_BUTTON_1               = 'Yes'
      TEXT_BUTTON_2               = 'Yes to all'
      DEFAULT_BUTTON              = '1'
      DISPLAY_CANCEL_BUTTON       = 'X'
    IMPORTING
      ANSWER                      = l_answer
            .
endform.
*\--------------------------------------------------------------------/
*/---------------------writeMessage-----------------------\
FORM writemessage USING value(p_type) TYPE sy-msgty
                        value(p_msg).
  CASE p_type.
    WHEN 'E' OR 'A' OR 'X'.
      WRITE / icon_led_red AS ICON.
    WHEN 'W'.
      WRITE / icon_led_yellow AS ICON.
    WHEN OTHERS.
      WRITE / icon_led_green AS ICON.
  ENDCASE.

  WRITE p_msg.
ENDFORM.                    "WriteMessage