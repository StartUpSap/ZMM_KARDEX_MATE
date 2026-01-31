//@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Data definitions Kardes'
//@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.query.implementedBy: 'ABAP:ZCL_CORE_KARDEX'

define root custom entity ZCD_KARDEX
  with parameters
    p_year  : gjahr,
    p_month : monat

{
  key COMPANYCODE                 : bukrs;
  key MATERIALDOCUMENTYEAR        : gjahr;
  key AccountingDocument          : belnr_d;
  key LedgerGLLineItem            : posnr;
      material                    : matnr;
      plant                       : werks_d;
      PostingDate                 : datum;
      ReferenceDocument           : awref;
      ReferenceDocumentItem       : posnr;
      CompanyCodeCurrency         : waers;
      GoodsMovementType           : bwart;
      DebitCreditCode             : shkzg;
      MaterialDocument            : mblnr;
      MATERIALDOCUMENTITEM        : mblpo;
      ProductType                 : mtart;
      ProductName                 : maktx;
      Quantity                    : abap.dec( 15, 3 );
      BaseUnit                    : meins;
      AmountInCompanyCodeCurrency : abap.dec( 23, 2 );
      FiscalPeriod                : monat;
      InitialQuantity             : abap.dec( 15, 3 );
      InitialValue                : abap.dec( 23, 2 );
      finalQuantity               : abap.dec( 15, 3 );
      finalalValue                : abap.dec( 23, 2 );
      
}
