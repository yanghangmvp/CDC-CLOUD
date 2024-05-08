@EndUserText.label: 'Parameter of Print'
define abstract entity ZR_ZT_PRINT_PARAMETER
{
  TemplateName           : abap.char(50);
  IsExternalProvidedData : abap_boolean;
  ExternalProvidedData   : abap.char(50);
  ProvidedKeys           : abap.sstring(256);
  SendToPrintQueue       : abap_boolean;
  PrintQueue             : abap.char( 32 );
  NumberOfCopies         : abap.int2;

}
