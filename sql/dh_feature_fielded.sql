create or replace view dh_feature_fielded as (
  select a.*, b.description_value as description,
    st_astext(geom.dh_geofield_geom) as dh_geofield,
    geom.dh_geofield_geom as dh_geofield_geom,
    nd.dh_nextdown_id_target_id as nextdown_id,
    p.dh_link_facility_mps_target_id as parent_id
  from dh_feature as a
  left outer join field_data_description as b
  on (
    b.entity_id = a.hydroid
    and b.entity_type = 'dh_feature'
  )
  left outer join field_data_dh_nextdown_id as nd
  on (
    nd.entity_id = a.hydroid
    and nd.entity_type = 'dh_feature'
  )
  left outer join field_data_dh_link_facility_mps as p
  on (
    p.entity_id = a.hydroid
    and p.entity_type = 'dh_feature'
  )
  left outer join field_data_dh_geofield as geom
  on (
    geom.entity_id = a.hydroid
    and geom.entity_type = 'dh_feature'
  )
);
