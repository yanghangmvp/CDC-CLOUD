@AccessControl.authorizationCheck: #CHECK
@Metadata.allowExtensions: true
@EndUserText.label: 'Projection View for ZR_ZT_PRINT_CONFIG'
define root view entity ZC_ZT_PRINT_CONFIG
  provider contract transactional_query
  as projection on ZR_ZT_PRINT_CONFIG
{
  key UUID,
      TemplateName,
      ServiceDefinitionName,
      IsExternalProvidedData,
      Template,
      MimeType,
      FileName,
      XsdFile,
      XsdType,
      XsdFileName,
      LocalLastChangedAt

}
