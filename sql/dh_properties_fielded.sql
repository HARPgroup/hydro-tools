create or replace view dh_properties_fielded as (
  select a.*, b.proptext_value as proptext,
    CASE
      WHEN c.field_dh_matrix_value IS NOT NULL THEN php_unserialize_to_json(c.field_dh_matrix_value )::text
      WHEN d.field_projection_table_value IS NOT NULL THEN php_unserialize_to_json(d.field_projection_table_value )::text
      ELSE NULL
    END as data_matrix,
    CASE
      WHEN c.field_dh_matrix_value IS NOT NULL THEN (php_unserialize_to_json(c.field_dh_matrix_value ) -> 'tabledata')::text
      WHEN d.field_projection_table_value IS NOT NULL THEN (php_unserialize_to_json(d.field_projection_table_value ) -> 'tabledata')::text
      ELSE NULL
    END as data_matrix_tabledata
  from dh_properties as a
  left outer join field_data_proptext as b
  on (
    b.entity_id = a.pid
    and b.entity_type = 'dh_properties'
  )
  left outer join field_data_field_dh_matrix as c
  on (
    c.entity_id = a.pid
    and c.entity_type = 'dh_properties'
  )
  left outer join field_data_field_projection_table as d
  on (
    d.entity_id = a.pid
    and d.entity_type = 'dh_properties'
  )
);
