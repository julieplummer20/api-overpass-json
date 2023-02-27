CLASS zcl_api_overpass_json_jrp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_api_overpass_json_jrp IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

    TRY.

        DATA(lo_destination) = cl_http_destination_provider=>create_by_comm_arrangement(
                                 comm_scenario  = 'Z_OVERPASS_JSON_CS_JRP'
                                 service_id     = 'Z_API_OVERPASS_JSON_JRP_REST'

                               ).

        DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination( i_destination = lo_destination ).
        DATA(lo_request) = lo_http_client->get_http_request( ).

        lo_request->set_query( query = 'data=[out:json];area[name="Heidelberg"];node["amenity"="biergarten"](area);out;' ).

        DATA(lo_response) = lo_http_client->execute( i_method = if_web_http_client=>get ).
        DATA(lv_json_results) = lo_response->get_text( ).

      CATCH cx_root INTO DATA(lx_exception).
        out->write( lx_exception->get_text( ) ).
    ENDTRY.


    TYPES:

      BEGIN OF ts_tags,
        name    TYPE string,
        website TYPE string,
      END OF ts_tags,

      BEGIN OF ts_element,
        type TYPE string,
        id   TYPE string,
        lat  TYPE string,
        lon  TYPE string,
        tags TYPE ts_tags,
      END OF ts_element,

      BEGIN OF ts_osm3s,
        timestamp_osm_base   TYPE string,
        timestamp_areas_base TYPE string,
        copyright            TYPE string,
      END OF ts_osm3s,

      BEGIN OF ts_osm,
        version   TYPE string,
        generator TYPE string,
        osm3s     TYPE ts_osm3s,
        elements  TYPE STANDARD TABLE OF ts_element WITH EMPTY KEY,
      END OF ts_osm.

    DATA ls_osm TYPE ts_osm.

    TRY.

        xco_cp_json=>data->from_string( lv_json_results )->apply( VALUE #(
          ( xco_cp_json=>transformation->pascal_case_to_underscore )
          ( xco_cp_json=>transformation->boolean_to_abap_bool )
        ) )->write_to( REF #( ls_osm ) ).

        out->write( | Names of beer gardens in Heidelberg (Germany) found on OpenStreetMaps. | ).
        out->write( | Data is obtained from the OSM API in JSON format and read into ABAP using the XCO library. | ).
        out->write( | Note: Not all locations have an associated name. | ).
        out->write( | Generator: { ls_osm-generator } | ).
        out->write( | ---------------------------------- | ).

        LOOP AT ls_osm-elements ASSIGNING FIELD-SYMBOL(<element>).
          out->write( | Beer garden number { sy-tabix }: { <element>-tags-name } | ).
        ENDLOOP.
        out->write( | ---------------------------------- | ).

* catch any error
      CATCH cx_root INTO DATA(lx_root).
        out->write( lx_root->get_text( ) ).
    ENDTRY.


  ENDMETHOD.

ENDCLASS.
