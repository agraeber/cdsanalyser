@AccessControl.authorizationCheck: #CHECK

@EndUserText.label: '##GENERATED CDS indexer'

define root view entity ZI_CDSFIELDINDEX
  as select from zcdsfieldindex as FieldIndex

{
  key entity_name               as EntityName,
  key element_name              as ElementName,

      base_object               as BaseObject,
      base_field                as BaseField,
      compatibility_contract    as CompatibilityContract,
      release_state             as ReleaseState,
      use_in_key_user_apps      as UseInKeyUserApps,
      use_in_sap_cloud_platform as UseInSapCloudPlatform,

      appcmp                    as ApplicationComponent,
      appcmpname                as ApplicationComponentName,
      appcmptext                as ApplicationComponentDescr,

      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      locallastchanged          as Locallastchanged,

      @Semantics.systemDateTime.lastChangedAt: true
      lastchanged               as Lastchanged
}
