@AccessControl.authorizationCheck: #CHECK

@EndUserText.label: 'Projection View for ZI_CDSFIELDINDEX'

@Metadata.allowExtensions: true

@ObjectModel.semanticKey: [ 'EntityName', 'ElementName' ]

define root view entity ZC_CDSFIELDINDEX
  provider contract transactional_query
  as projection on ZI_CDSFIELDINDEX

{
  key EntityName,
  key ElementName,

      BaseObject,
      BaseField,
      CompatibilityContract,
      ApplicationComponent,
      ApplicationComponentDescr,
      ApplicationComponentName,
      Locallastchanged
}
