@AccessControl.authorizationCheck: #CHECK
@Metadata.allowExtensions: true
@EndUserText.label: 'Projection View for ZI_CDSFIELDINDEX'
@ObjectModel.semanticKey: [ 'EntityName', 'ElementName' ]
define root view entity ZC_CDSFIELDINDEX
  provider contract transactional_query
  as projection on ZI_CDSFIELDINDEX
{
  key entityname,
  key elementname,
  baseobject,
  basefield,
  compatibilitycontract,
  locallastchanged
  
}
