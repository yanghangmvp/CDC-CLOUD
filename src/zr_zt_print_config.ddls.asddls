@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: '##GENERATED ZZT_PRINT_CONFIG'
define root view entity ZR_ZT_PRINT_CONFIG
  as select from zzt_print_config
{
  key uuid                      as UUID,
      template_name             as TemplateName,
      service_definition_name   as ServiceDefinitionName,
      is_external_provided_data as IsExternalProvidedData,

      // 设置为文件模板，存放Adobe liveCycle 导出的打印文件
      @Semantics.largeObject:
      { mimeType: 'MimeType',
      fileName: 'FileName',
      contentDispositionPreference: #ATTACHMENT }
      template                  as Template,
      @Semantics.mimeType: true
      mime_type                 as MimeType,
      file_name                 as FileName,

      // 设置为附加文件，用于记录创建时产生对应的数据源链接文件，为后续Adobe 开发提供文件
      @Semantics.largeObject:
      { mimeType: 'XsdType',
      fileName: 'XsdFileName',
      contentDispositionPreference: #ATTACHMENT }
      xsd_file                  as XsdFile,
      @Semantics.mimeType: true
      xsd_type                  as XsdType,
      xsd_file_name             as XsdFileName,
      @Semantics.user.createdBy: true
      created_by                as CreatedBy,
      @Semantics.systemDateTime.createdAt: true
      created_at                as CreatedAt,
      @Semantics.user.lastChangedBy: true
      last_changed_by           as LastChangedBy,
      @Semantics.systemDateTime.lastChangedAt: true
      last_changed_at           as LastChangedAt,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      local_last_changed_at     as LocalLastChangedAt

}
