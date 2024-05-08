@AccessControl.authorizationCheck: #CHECK
@Metadata.allowExtensions: true
@EndUserText.label: 'Projection View for ZR_ZT_PRINT_LOG'
define root view entity ZC_ZT_PRINT_LOG
  provider contract transactional_query
  as projection on ZR_ZT_PRINT_LOG
{
  key UUID,
      TemplateUUID,
      IsExternalProvidedData,
      ExternalProvidedData,
      ProvidedKeys,
      Pdf,
      MimeType,
      FileName,
      SendToPrintQueue,
      NumberOfCopies,
      PrintQueue,
      MimeTypeData,
      DataFileName,
      LocalLastChangedAt,
      _Config : redirected to ZC_ZT_PRINT_CONFIG
}
