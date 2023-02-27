CLASS zcl_api_overpass_xml_000 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_api_overpass_xml_000 IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
    " Create HTTP client
    TRY.

        DATA(lo_destination) = cl_http_destination_provider=>create_by_comm_arrangement(
                                 comm_scenario  = 'ZMDC_OVERPASS_CSCEN'
                                 service_id     = 'ZMDC_OVERPASS_OBS_REST'
                               ).

        DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination( i_destination = lo_destination ).
        DATA(lo_request) = lo_http_client->get_http_request( ).

        " Send request; store response in lv_xml_results
        lo_request->set_query( query = 'data=area[name="Heidelberg"];node["amenity"="biergarten"](area);out;' ).

        DATA(lo_response) = lo_http_client->execute( i_method = if_web_http_client=>get ).
        DATA(lv_xml_results) = lo_response->get_text( ).

      CATCH cx_root INTO DATA(lx_exception).
        out->write( lx_exception->get_text( ) ).
    ENDTRY.

    " Create structures etc
    TYPES:
      BEGIN OF ts_tag,
        property TYPE string,
        value    TYPE string,
      END OF ts_tag,

      BEGIN OF ts_node,
        id        TYPE string,
        latitude  TYPE string,
        longitude TYPE string,
        tags      TYPE STANDARD TABLE OF ts_tag WITH EMPTY KEY,
      END OF ts_node,

      BEGIN OF ts_metadata,
        osm_base  TYPE string,
        osm_areas TYPE string,
      END OF ts_metadata,

      BEGIN OF ts_osm,
        api_version   TYPE string,
        api_generator TYPE string,
        note          TYPE string,
        metadata      TYPE ts_metadata,
        nodes         TYPE STANDARD TABLE OF ts_node WITH EMPTY KEY,
      END OF ts_osm.

    DATA ls_osm TYPE ts_osm.


    TRY.
        " transform response; store transformed response in structure osm
        CALL TRANSFORMATION zmdc_osm_xml_st
        SOURCE XML lv_xml_results
        RESULT osm = ls_osm.


        out->write( | Names of beergardens in Heidelberg (Germany) found on OpenStreetMaps. | ).
        out->write( | Data is obtained from the OSM API in XML format and read into ABAP using a simple transformation (ST). | ).
        out->write( | Note: Not all locations have an associated name. | ).
        out->write( | Generator: { ls_osm-api_generator } | ).
        out->write( | ---------------------------------- | ).

        " Output data to console
        DATA lv_index TYPE int4.
        LOOP AT ls_osm-nodes ASSIGNING FIELD-SYMBOL(<node>).
          lv_index = sy-tabix.
          LOOP AT <node>-tags ASSIGNING FIELD-SYMBOL(<tag>).
            IF <tag>-property = 'name'.
              out->write( | Beergarden number { lv_index }: { <tag>-value } | ).
            ENDIF.
          ENDLOOP.
        ENDLOOP.
        out->write( | ---------------------------------- | ).

        " catch any error
      CATCH cx_root INTO DATA(lx_root).
        out->write( lx_root->get_text( ) ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
