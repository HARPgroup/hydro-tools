create or replace view dh_adminreg_feature_fielded as (
  select a.*, b.description_value as description,
  p.dh_link_admin_submittal_pr_target_id as parent_id,
  pm.field_dha_link_modification_target_id as modifies_id,
  f.dh_link_feature_submittal_target_id as feature_id
  from dh_adminreg_feature as a
  left outer join field_data_description as b
  on (
    b.entity_id = a.adminid
    and b.entity_type = 'dh_adminreg_feature'
  )
  left outer join field_data_dh_link_admin_submittal_pr as p
  on (
    p.entity_id = a.adminid
    and p.entity_type = 'dh_adminreg_feature'
  )
  left outer join field_data_dh_link_feature_submittal as f 
  on (
    f.entity_id = a.adminid
    and f.entity_type = 'dh_adminreg_feature'
  )
  left outer join field_data_field_dha_link_modification as pm
  on (
    pm.entity_id = a.adminid
    and pm.entity_type = 'dh_adminreg_feature'
  )
);
