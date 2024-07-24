@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: '##GENERATED ZZT_PRINT_LOG'
define root view entity ZR_ZT_PRINT_LOG
  as select from zzt_print_log as Log
  association [0..1] to ZR_ZT_PRINT_CONFIG as _Config on $projection.TemplateUUID = _Config.UUID
{
  key uuid                      as UUID,
      @Consumption.valueHelpDefinition: [{  entity: {   name: 'ZC_ZT_PRINT_CONFIG' ,
                                                        element: 'UUID'  }     }]
      template_uuid             as TemplateUUID,
      templatename              as TemplateName,
      is_external_provided_data as IsExternalProvidedData,
      @Semantics.largeObject:
      {
        mimeType: 'MimeTypeData',
        fileName: 'DataFileName',
        contentDispositionPreference: #ATTACHMENT }
      external_provided_data    as ExternalProvidedData,
      @Semantics.mimeType: true
      mime_type_data            as MimeTypeData,
      data_file_name            as DataFileName,
      provided_keys             as ProvidedKeys,
      @Semantics.largeObject:
      { mimeType: 'MimeType',
      fileName: 'FileName',
      contentDispositionPreference: #ATTACHMENT }
      pdf                       as Pdf,
      @Semantics.mimeType: true
      mime_type                 as MimeType,
      file_name                 as FileName,
      send_to_print_queue       as SendToPrintQueue,
      number_of_copies          as NumberOfCopies,
      print_queue               as PrintQueue,
      @Semantics.user.createdBy: true
      created_by                as CreatedBy,
      @Semantics.systemDateTime.createdAt: true
      created_at                as CreatedAt,
      @Semantics.user.lastChangedBy: true
      last_changed_by           as LastChangedBy,
      @Semantics.systemDateTime.lastChangedAt: true
      last_changed_at           as LastChangedAt,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      local_last_changed_at     as LocalLastChangedAt,
      _Config
}
