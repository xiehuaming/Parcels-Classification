; Add the extension to the toolbox. Called automatically on ENVI startup.
pro Area_of_winter_wheat_extensions_init

  ; Set compile options
  compile_opt IDL2

  ; Get ENVI session
  e = ENVI(/CURRENT)

  ; Add the extension to a subfolder
  e.AddExtension, 'Area_of_winter_wheat', 'Area_of_winter_wheat', PATH=''
end

pro Area_of_winter_wheat_event, ev

  compile_opt idl2
  WIDGET_CONTROL, ev.TOP, GET_UVALUE=pState

  uname = WIDGET_INFO(ev.ID, /UNAME)
  case uname of
    'next': begin
     
      ;�趨�ٽ�ֵ
      common set_value,min_b1,max_b1,min_b2,max_b2,min_b3,max_b3,min_b4,max_b4,min_NDVI,max_NDVI,fx
      
      WIDGET_CONTROL,pState.MIN_b1,GET_VALUE=min_b1
      min_b1=min_b1
      WIDGET_CONTROL,pState.MAX_b1,GET_VALUE=max_b1
      max_b1=max_b1
      WIDGET_CONTROL,pState.MIN_b2,GET_VALUE=min_b2
      min_b2=min_b2
      WIDGET_CONTROL,pState.MAX_b2,GET_VALUE=max_b2
      max_b2=max_b2
      WIDGET_CONTROL,pState.MIN_b3,GET_VALUE=min_b3
      min_b3=min_b3
      WIDGET_CONTROL,pState.MAX_b3,GET_VALUE=max_b3
      max_b3=max_b3
      WIDGET_CONTROL,pState.MIN_b4,GET_VALUE=min_b4
      min_b4=min_b4
      WIDGET_CONTROL,pState.MAX_b4,GET_VALUE=max_b4
      max_b4=max_b4
      min_NDVI=0
      max_NDVI=1
      WIDGET_CONTROL,pState.FX,GET_VALUE=fx
      ;����Ϊ�Ƿ�Ϊ���ؿ���ж�ֵ����Ϊ���ηָ���������������ȡģ���������fxֵ
      fx=fx
      Raster = pState.RASTER
      shpFile = pState.SHPFILE
      inFile=pState.INFILE
      
      ;��·����pro wheat_shp1����Ϊ��һ���жϳ���С��ؿ����ݣ���pro wheat_shp��Ϊ���ս���Ļ��ܽ��
      wheatshp_file=pState.WHEATSHP_FILE
       WIDGET_CONTROL, ev.TOP, /DESTROY
       
      ;��������ĸ���·��     
      potPos = STRPOS(shpFile,'.',/reverse_search)
      prjfile = STRMID(shpFile,0,potPos[0])+'.prj'
      prjbasename=file_baseName(prjfile)
      testfile='c:\test\'+prjbasename
      wheatshp_path=file_dirName(wheatshp_file)
      tempprj = wheatshp_path+'\'+prjbasename
      wheatpotPos = STRPOS(wheatshp_file,'.',/reverse_search)
      wheatprjfile = STRMID(wheatshp_file,0,wheatpotPos[0])+'.prj'
      wheatprj=file_baseName(wheatprjfile)
      
      ;������ʱ�ļ���
      spawn,'md C:\test',/hide   
      spawn,['copy', prjfile,' c:\test\'],/hide
      spawn,['rename',testfile,'test1.prj'],/hide
      spawn,['copy', prjfile,' c:\test\'],/hide
      spawn,['rename',testfile,' cutshape.prj'],/hide
      spawn,['copy', prjfile,wheatshp_path],/hide
      spawn,['rename',tempprj,wheatprj],/hide
      
      ;���Ա�����ң��Ӱ�������Ϣ�ĵؿ�����
      shapefile='c:\test\test1.shp'
      
      ;rule�ļ��洢λ��
      savepath='c:/test/wheat.rul'
      
      ;���ڶ��ηָ�����ĵؿ�����
      cutshapefile='c:\test\cutshape.shp'          

      ;��ң��Ӱ��Ĺ�����Ϣ��ӵ�ʸ���ļ������Ա���
      Areaofwinterwheat, Raster, shpFile,inFile,shapefile,$
  min_b1,max_b1,$
  min_b2,max_b2,$
  min_b3,max_b3,$
  min_b4,max_b4
          
      ;������Ҫ���ηָ������ʸ���ļ�
      cutshape,shapefile=shapefile,cutshapefile=cutshapefile,fx=fx
      ;��ȡ�����ؿ��е�С�󲿷�
      wheat_shp1,shapefile=shapefile,wheatshp_file=wheatshp_file,fx=fx
  
      resultfile='c:\test\test.img'
      ;�ü��� ����Ҫ���ηָ���ȡ��Ӱ������������������ȡ�������ң��Ӱ��
      cal_subset,infile=infile, shapefile=cutshapefile, resultfile=resultfile


      ;��������Ҫ���ηָ���ȡ��Ӱ������������������ȡ��������Ҫ�Ĺ����ļ�
      writerule,min_b1,max_b1,min_b2,max_b2,min_b3,max_b3,min_b4,max_b4,savepath

      rule_file='c:\test\wheat.rul'
      vector_filename='c:\test\vector.shp'
      ;����Ҫ���ηָ���ȡ��Ӱ������������������ȡ
      EXAMPLE_FX_RULEBASED_DOIT,file=resultfile,rule_file=rule_file,vector_filename=vector_filename

      ;����ϵؿ��е�С�󲿷��봿�ؿ��е�С�󲿷ֽ��кϲ��������յ�С��ؿ��ļ�
      wheat_shp,wheatshp_file=wheatshp_file,vector_filename=vector_filename

      ;�Ӵ������Ƴ�Ӱ������
      Raster.Close

      ;ɾ����ʱ�ļ���(����鿴�������ݿɱ������ļ���)
      spawn,'rd /q /s C:\test',/hide

      
    end

    'Cancel': begin
      WIDGET_CONTROL, ev.TOP, /DESTROY
      RETURN
    end

    else:
  endcase

end


; ENVI Extension code. Called when the toolbox item is chosen.
pro Area_of_winter_wheat

  ; Set compile options
  compile_opt IDL2

  ; General error handler
  CATCH, err
  if (err ne 0) then begin
    CATCH, /CANCEL
    if OBJ_VALID(e) then $
      e.ReportError, 'ERROR: ' + !error_state.msg
    MESSAGE, /RESET
    return
  endif

  ;Get ENVI session
  e = ENVI(/CURRENT)

  ;******************************************
  ; Insert your ENVI Extension code here...
  ;******************************************
  ;ѡ��ң��Ӱ��
  UI = e.UI
  Raster = UI.SelectInputData(/RASTER, Bands=Bands, Sub_RECT=Sub_RECT, $
    title = 'ѡ��ң��Ӱ��', /DISABLE_NO_DATA)
  if Raster eq !NULL then RETURN
  inFile = Raster.URI
  Raster = ENVISUBSETRASTER(Raster, Bands=Bands, SUB_RECT=Sub_RECT)

  ;��ʾң��Ӱ��
  view=e.getview()
  layer1=view.createlayer(Raster,/cir)


  ;ѡ��ؿ����ݣ�shp�ļ���
  Vector = UI.SelectInputData(/VECTOR, title='ѡ��ؿ�����',$
    /DISABLE_NO_DATA)
  if Vector eq !NULL then RETURN
  shpFile = Vector.URI

  ;��ʾ�ؿ�����
  oshp=e.OpenVector(shpfile)
  layer2=view.createlayer(oshp,/cir)


  ENVI_CENTER, xoff, yoff
  tlb = WIDGET_BASE(TITLE='С��ؿ�', /COLUMN, $
    XOFFSET=xoff, YOFFSET=yoff, TLB_FRAME_ATTR=1)

  ;�ļ�·����ʾ
  filebase=WIDGET_BASE(tlb, /column, /FRAME)
  label1=widget_label(filebase,value='ң��Ӱ��'+inFile)
  label2=widget_label(filebase,value='�ؿ����ݣ�'+shpFile)
  
  ;������·��
  wheat_file=dialog_pickfile(title='��ѡ�������·��',filter='*.shp')
  wheatshp_file=wheat_file+'.shp'
  ;��д������д�������ε����ֵ����Сֵ��
  rulebase=WIDGET_BASE(tlb, /column, /FRAME)
  ;��һ����
  rulebase1=WIDGET_BASE(rulebase, /ROW, /FRAME)
  rulelabel1=widget_label(rulebase1,value='��һ���Σ�')
  min_b1 = CW_FIELD(rulebase1,TITLE = "��Сֵ��",/FRAME,VALUE=260, /INTEGER)
  max_b1 = CW_FIELD(rulebase1,TITLE = "���ֵ��",/FRAME,VALUE=290, /INTEGER)
  ;�ڶ�����
  rulebase2=WIDGET_BASE(rulebase, /ROW, /FRAME)
  rulelabel2=widget_label(rulebase2,value='�ڶ����Σ�')
  min_b2 = CW_FIELD(rulebase2,TITLE = "��Сֵ��",/FRAME,VALUE=227, /INTEGER)
  max_b2 = CW_FIELD(rulebase2,TITLE = "���ֵ��",/FRAME,VALUE=279, /INTEGER)
  ;��������
  rulebase3=WIDGET_BASE(rulebase, /ROW, /FRAME)
  rulelabel3=widget_label(rulebase3,value='�������Σ�')
  min_b3 = CW_FIELD(rulebase3,TITLE = "��Сֵ��",/FRAME,VALUE=190, /INTEGER)
  max_b3 = CW_FIELD(rulebase3,TITLE = "���ֵ��",/FRAME,VALUE=251, /INTEGER)
  ;���Ĳ���
  rulebase4=WIDGET_BASE(rulebase, /ROW, /FRAME)
  rulelabel4=widget_label(rulebase4,value='���Ĳ��Σ�')
  min_b4 = CW_FIELD(rulebase4,TITLE = "��Сֵ��",/FRAME,VALUE=282, /INTEGER)
  max_b4 = CW_FIELD(rulebase4,TITLE = "���ֵ��",/FRAME,VALUE=397, /INTEGER)
  ;��ɢ��
  rulebasefx=WIDGET_BASE(rulebase, /ROW, /FRAME)
  fx = CW_FIELD(rulebasefx,TITLE = "��ɢ�ȣ�",/FRAME,VALUE=57, /INTEGER)


  ;��һ����ť
  okBase = WIDGET_BASE(tlb, /ROW, /FRAME)
  okBtn = WIDGET_BUTTON(okBase, value='��һ��', uname='next', XSIZE=60)
  cancelBtn = WIDGET_BUTTON(okBase, value='ȡ��',uname='Cancel',XSIZE=80)

  WIDGET_CONTROL, tlb, /REALIZE


  pState = {                $
    RASTER:Raster,          $
    INFILE:inFile,          $
    SHPFILE:shpFile,        $
    WHEATSHP_FILE:wheatshp_file,$
    MIN_B1:min_b1,$
    MAX_B1:max_b1,$
    MIN_B2:min_b2,$
    MAX_B2:max_b2,$
    MIN_B3:min_b3,$
    MAX_B3:max_b3,$
    MIN_B4:min_b4,$
    MAX_B4:max_b4,$
    FX:fx}
  

  WIDGET_CONTROL, tlb, SET_UVALUE=pState

  XMANAGER, 'Area_of_winter_wheat', tlb, /NO_BLOCK

end




;��ң��Ӱ��Ĺ�����Ϣ��ӵ�ʸ���ļ������Ա���
pro Areaofwinterwheat, Raster, shpFile, inFile,shapefile,$
  min_b1,max_b1,$
  min_b2,max_b2,$
  min_b3,max_b3,$
  min_b4,max_b4

  compile_opt idl2
  e = ENVI(/current)
  CATCH, err
  if (err ne 0) then begin
    CATCH, /CANCEL
    if OBJ_VALID(e) then $
      e.ReportError, 'ERROR: ' + !ERROR_STATE.MSG
    MESSAGE, /RESET
    RETURN
  endif



  ;common set_value,x
  x=0



  fid = ENVIRASTERTOFID(raster)
  ENVI_FILE_QUERY, fid, ns=ns, nl=nl, nb=nb, dims=dims

  ;����shp
  shapefile=shapefile
  newshp=obj_new('IDLffshape',shapefile,Entity_type=5,/update)
  ;�������Ա�ṹ
  newshp->AddAttribute,'ID',3,8,PRECISION=0
  newshp->AddAttribute,'Class_Name',7,30,PRECISION=0
  newshp->AddAttribute,'Counts',5,16,PRECISION=6
  newshp->AddAttribute,'B1_Min',5,16,PRECISION=6
  newshp->AddAttribute,'B1_Max',5,16,PRECISION=6
  newshp->AddAttribute,'B1_Mean',5,16,PRECISION=6
  newshp->AddAttribute,'B1_stddev',5,16,PRECISION=6
  newshp->AddAttribute,'B2_Min',5,16,PRECISION=6
  newshp->AddAttribute,'B2_Max',5,16,PRECISION=6
  newshp->AddAttribute,'B2_Mean',5,16,PRECISION=6
  newshp->AddAttribute,'B2_stddev',5,16,PRECISION=6
  newshp->AddAttribute,'B3_Min',5,16,PRECISION=6
  newshp->AddAttribute,'B3_Max',5,16,PRECISION=6
  newshp->AddAttribute,'B3_Mean',5,16,PRECISION=6
  newshp->AddAttribute,'B3_stddev',5,16,PRECISION=6
  newshp->AddAttribute,'B4_Min',5,16,PRECISION=6
  newshp->AddAttribute,'B4_Max',5,16,PRECISION=6
  newshp->AddAttribute,'B4_Mean',5,16,PRECISION=6
  newshp->AddAttribute,'B4_stddev',5,16,PRECISION=6
  newshp->AddAttribute,'NDVI',5,16,PRECISION=6

  entNew = {IDL_SHAPE_ENTITY}
  entNew.SHAPE_TYPE = 5
  attr = newshp->GetAttributes(/ATTRIBUTE_STRUCTURE)
  newshp->AddAttribute,'ID',3,8,PRECISION=0

  ;��ȡshp�ļ�����Ϣ
  oshp=OBJ_NEW('IDLffShape',shpFile)
  oshp->GETPROPERTY,n_entities=n_ent

  iProj = ENVI_PROJ_CREATE(/geographic)
  ;�Զ���ȡprj�ļ���ȡͶӰ����ϵ
  potPos = STRPOS(shpFile,'.',/reverse_search)  
  prjfile = STRMID(shpFile,0,potPos[0])+'.prj'

  if FILE_TEST(prjfile) then begin
    OPENR, lun, prjFile, /GET_LUN
    strprj = ''
    READF, lun, strprj
    FREE_LUN, lun

    case STRMID(strprj, 0,6) of
      'GEOGCS': begin
        iProj = ENVI_PROJ_CREATE(PE_COORD_SYS_STR=strprj, $
          type = 1)
      end
      'PROJCS': begin
        iProj = ENVI_PROJ_CREATE(PE_COORD_SYS_STR=strprj, $
          type = 42)
      end
    endcase
  endif

  oProj = ENVI_GET_PROJECTION(fid = fid)


  statsMIN = !NULL
  statsMAX = !NULL
  statsMEAN = !NULL
  statsCOUNTS = !NULL
  statsSTDDEV = !NULL


  ENVI_REPORT_INIT, ['ң��Ӱ��: '+inFile, '�ؿ�����: '+shpFile], $
    title="С�����", $
    base=base

  ENVI_REPORT_INC, base, n_ent

  ;ѭ���У�ʹ��ÿ��shp��¼������roi
  ;Ȼ��ʹ��ROI������Ĥͳ��
  for i = 0, n_ent-1 do begin
    ;
    ent = oshp->GETENTITY(i, /ATTRIBUTES) ;��i����¼
    N_VERTICES=ent.N_VERTICES ;�������
    parts=*(ent.PARTS)
    verts=*(ent.VERTICES)

    ; ����������ת��Ϊ�����ļ��ĵ�������
    ENVI_CONVERT_PROJECTION_COORDINATES,  $
      verts[0,*], verts[1,*], iProj,    $
      oXmap, oYmap, oProj
    ; ת��Ϊ�ļ�����
    ENVI_CONVERT_FILE_COORDINATES,fid,    $
      xFile,yFile,oXmap,oYmap

    xFile = xFile > 0 < ns
    yFile = yFile > 0 < nl

    sub_Rect = LONG64([MIN(xFile),MIN(yFile),MAX(xFile),MAX(yFile)])

    subRaster = ENVISUBSETRASTER(Raster, SUB_RECT=sub_Rect)
    subFid = ENVIRASTERTOFID(subRaster)
    ENVI_FILE_QUERY, subFid, ns=subNS, nl=subNL
    ; ת��Ϊ�ļ�����
    ENVI_CONVERT_FILE_COORDINATES, subFid,    $
      xFile,yFile,oXmap,oYmap

    xFile = LONG64(xFile)
    yFile = LONG64(yFile)

    ;����ROI
    N_Parts = N_ELEMENTS(Parts)
    roi_ids = !NULL
    for j=0, N_Parts-1 do begin
      roi_id = ENVI_CREATE_ROI(color=i,     $
        ns = subNS ,  nl = subNL)
      if j eq N_Parts-1 then begin
        tmpFileX = xFile[Parts[j]:*]
        tmpFileY = yFile[Parts[j]:*]
      endif else begin
        tmpFileX = xFile[Parts[j]:Parts[j+1]-1]
        tmpFileY = yFile[Parts[j]:Parts[j+1]-1]
      endelse

      ENVI_DEFINE_ROI, roi_id, /polygon,    $
        xpts=REFORM(tmpFileX), ypts=REFORM(tmpFileY)

      ;����е�ROI��Ԫ��Ϊ0���򲻱���
      ENVI_GET_ROI_INFORMATION, roi_id, NPTS=npts
      if npts eq 0 then continue

      roi_ids = [roi_ids, roi_id]
    endfor

    ENVI_REPORT_STAT,base, i, n_ent

    ; �ж�ROI���� 20210907
    if roi_ids eq !NULL then continue
    ; �ж�ROI���� end 
    
    ;    name = Attrs[attrIdx, i]
    roiFile = e.GetTemporaryFilename('roi')
    ENVI_SAVE_ROIS, roiFile, roi_ids

    ;����ENVI�ӿڽ�����Ĥ��ͳ��
    roi = e.OpenROI(roiFile)
    rasterWithMask = ENVIROIMASKRASTER(SubRaster, roi)
    ;���β�Ϊ1ʱ������ͳ��COUNTS
    stats = ENVIRasterStatistics(ENVISUBSETRASTER(rasterWithMask,bands=[0]), $
      /HISTOGRAMS, HISTOGRAM_NBINS=1)
    Hist = stats.HISTOGRAMS
    Hist = Hist[0]
    COUNTS = Hist.COUNTS

    ;ͳ��min��max��mean��ֵ
    stats = ENVIRasterStatistics(rasterWithMask)
    NDVImean=(stats.MEAN[3]-stats.MEAN[2])/(stats.MEAN[3]+stats.MEAN[2])

    ;����shp
    entNew.ISHAPE=x
    entNew.BOUNDS = [min(verts[0,*]),min(verts[1,*]),0,0,max(verts[0,*]),max(verts[1,*]),0,0]
    pvertice=verts
    entnew.VERTICES=PTR_NEW(pvertice,/no_copy)
    entNew.N_Parts=N_Parts
    entnew.PARTS=PTR_NEW(parts,/no_copy)
    entNew.N_VERTICES = N_VERTICES
    newshp->PutEntity, entNew

    attr.ATTRIBUTE_0 = x
    ;�ж��Ƿ���С��
    if stats.MEAN[0] ge min_b1 then begin
      if stats.MEAN[0] le max_b1 then begin
        if stats.MEAN[1] ge min_b2 then begin
          if stats.MEAN[1] le max_b2 then begin
            if stats.MEAN[2] ge min_b3 then begin
              if stats.MEAN[2] le max_b3 then begin
                if stats.MEAN[3] ge min_b4 then begin
                  if stats.MEAN[3] le max_b4 then begin
                    ;if NDVIMEAN ge min_NDVI then begin
                     ; if NDVIMEAN le max_NDVI then begin
                        attr.attribute_1='wheat'
                      ;endif else begin
                       ; attr.attribute_1='other'
                      ;endelse
                   ; endif else begin
                     ; attr.attribute_1='other'
                  ;  endelse
                  endif else begin
                    attr.attribute_1='other'
                  endelse
                endif else begin
                  attr.attribute_1='other'
                endelse
              endif else begin
                attr.attribute_1='other'
              endelse
            endif else begin
              attr.attribute_1='other'
            endelse
          endif else begin
            attr.attribute_1='other'
          endelse
        endif else begin
          attr.attribute_1='other'
        endelse
      endif else begin
        attr.attribute_1='other'
      endelse
    endif else begin
      attr.attribute_1='other'
    endelse

    attr.attribute_2=COUNTS
    attr.attribute_3=stats.MIN[0]
    attr.attribute_4=stats.MAX[0]
    attr.attribute_5=stats.MEAN[0]
    attr.attribute_6=stats.STDDEV[0]

    attr.attribute_7=stats.MIN[1]
    attr.attribute_8=stats.MAX[1]
    attr.attribute_9=stats.MEAN[1]
    attr.attribute_10=stats.STDDEV[1]

    attr.attribute_11=stats.MIN[2]
    attr.attribute_12=stats.MAX[2]
    attr.attribute_13=stats.MEAN[2]
    attr.attribute_14=stats.STDDEV[2]

    attr.attribute_15=stats.MIN[3]
    attr.attribute_16=stats.MAX[3]
    attr.attribute_17=stats.MEAN[3]
    attr.attribute_18=stats.STDDEV[3]

    attr.attribute_19=NDVImean




    newshp->setattributes,x,attr
    x=x+1

    rasterWithMask.Close
    SubRaster.Close
    ENVI_FILE_MNG, id=subFid, /remove
    foreach element, roi do element.CLOSE
    FILE_DELETE, roiFile, /QUIET

  endfor

  OBJ_DESTROY, oshp
  OBJ_DESTROY, newshp
  ENVI_REPORT_INIT, base=base, /finish

end



;��ȡ����Ҫ�ü�Ӱ�������shp�ļ�
pro cutshape,shapefile=shapefile,cutshapefile=cutshapefile,fx=fx

  compile_opt idl2
  e = ENVI(/current)
  CATCH, err
  if (err ne 0) then begin
    CATCH, /CANCEL
    if OBJ_VALID(e) then $
      e.ReportError, 'ERROR: ' + !ERROR_STATE.MSG
    MESSAGE, /RESET
    RETURN
  endif

  ;�趨�ٽ�ֵ
  y=0
  fx=fx
  ;����shp
  cutshapefile=cutshapefile
  cutshp=obj_new('IDLffshape',cutshapefile,Entity_type=5,/update)
  ;�������Ա�ṹ
  cutshp->AddAttribute,'ID',3,8,PRECISION=0

  entcut = {IDL_SHAPE_ENTITY}
  entcut.SHAPE_TYPE = 5
  cutattr = cutshp->GetAttributes(/ATTRIBUTE_STRUCTURE)
  cutshp->AddAttribute,'ID',3,8,PRECISION=0

  ;��ȡshp�ļ�����Ϣ
  oshp=OBJ_NEW('IDLffShape',shapefile, /update)
  oshp->GETPROPERTY,n_entities=n_ent

  iProj = ENVI_PROJ_CREATE(/geographic)
  ;�Զ���ȡprj�ļ���ȡͶӰ����ϵ
  potPos = STRPOS(shapefile,'.',/reverse_search)  ;
  prjfile = STRMID(shapefile,0,potPos[0])+'.prj'

  if FILE_TEST(prjfile) then begin
    OPENR, lun, prjFile, /GET_LUN
    strprj = ''
    READF, lun, strprj
    FREE_LUN, lun

    case STRMID(strprj, 0,6) of
      'GEOGCS': begin
        iProj = ENVI_PROJ_CREATE(PE_COORD_SYS_STR=strprj, $
          type = 1)
      end
      'PROJCS': begin
        iProj = ENVI_PROJ_CREATE(PE_COORD_SYS_STR=strprj, $
          type = 42)
      end
    endcase
  endif

  for i = 0, n_ent-1 do begin
    ;
    ent = oshp->GETENTITY(i, /ATTRIBUTES) ;��i����¼
    N_VERTICES=ent.N_VERTICES ;�������
    parts=*(ent.PARTS)
    verts=*(ent.VERTICES)
    N_Parts = N_ELEMENTS(Parts)
    attr=oshp->getattributes(i)

    if attr.attribute_6 ge fx then begin
      ;����shp
      entcut.ISHAPE=y
      entcut.BOUNDS = [min(verts[0,*]),min(verts[1,*]),0,0,max(verts[0,*]),max(verts[1,*]),0,0]
      pvertice=verts
      entcut.VERTICES=PTR_NEW(pvertice,/no_copy)
      entcut.N_Parts=N_Parts
      entcut.PARTS=PTR_NEW(parts,/no_copy)
      entcut.N_VERTICES = N_VERTICES
      cutshp->PutEntity, entcut

      cutattr.ATTRIBUTE_0 = y


      cutshp->setattributes,y,cutattr
      y=y+1
    endif

  endfor

  OBJ_DESTROY, oshp
  OBJ_DESTROY, cutshp
  ;  ENVI_REPORT_INIT, base=base, /finish

end

;��ȡ�����λ�õ�С�����shp�ļ�
pro wheat_shp1,shapefile=shapefile,wheatshp_file=wheatshp_file,fx=fx

  compile_opt idl2
  e = ENVI(/current)
  CATCH, err
  if (err ne 0) then begin
    CATCH, /CANCEL
    if OBJ_VALID(e) then $
      e.ReportError, 'ERROR: ' + !ERROR_STATE.MSG
    MESSAGE, /RESET
    RETURN
  endif

  ;�趨�ٽ�ֵ
  y=0
  fx=fx
  ;����shp
  wheatshp_file=wheatshp_file
  wheatshp1=obj_new('IDLffshape',wheatshp_file,Entity_type=5,/update)
  ;�������Ա�ṹ
  wheatshp1->AddAttribute,'ID',3,8,PRECISION=0

  entcut = {IDL_SHAPE_ENTITY}
  entcut.SHAPE_TYPE = 5
  cutattr = wheatshp1->GetAttributes(/ATTRIBUTE_STRUCTURE)
  wheatshp1->AddAttribute,'ID',3,8,PRECISION=0

  ;��ȡshp�ļ�����Ϣ
  oshp=OBJ_NEW('IDLffShape',shapefile, /update)
  oshp->GETPROPERTY,n_entities=n_ent

  iProj = ENVI_PROJ_CREATE(/geographic)
  ;�Զ���ȡprj�ļ���ȡͶӰ����ϵ
  potPos = STRPOS(shapefile,'.',/reverse_search)  ;
  prjfile = STRMID(shapefile,0,potPos[0])+'.prj'

  if FILE_TEST(prjfile) then begin
    OPENR, lun, prjFile, /GET_LUN
    strprj = ''
    READF, lun, strprj
    FREE_LUN, lun

    case STRMID(strprj, 0,6) of
      'GEOGCS': begin
        iProj = ENVI_PROJ_CREATE(PE_COORD_SYS_STR=strprj, $
          type = 1)
      end
      'PROJCS': begin
        iProj = ENVI_PROJ_CREATE(PE_COORD_SYS_STR=strprj, $
          type = 42)
      end
    endcase
  endif

  for i = 0, n_ent-1 do begin
    ;
    ent = oshp->GETENTITY(i, /ATTRIBUTES) ;��i����¼
    N_VERTICES=ent.N_VERTICES ;�������
    parts=*(ent.PARTS)
    verts=*(ent.VERTICES)
    N_Parts = N_ELEMENTS(Parts)
    attr=oshp->getattributes(i)
    ;��ɢ�ȣ���һ���α�׼��ж��Ƿ�Ϊ��ϵؿ�
    if attr.attribute_6 lt fx then begin
      if attr.attribute_1 eq 'wheat' then begin
        ;����shp
        entcut.ISHAPE=y
        entcut.BOUNDS = [min(verts[0,*]),min(verts[1,*]),0,0,max(verts[0,*]),max(verts[1,*]),0,0]
        pvertice=verts
        entcut.VERTICES=PTR_NEW(pvertice,/no_copy)
        entcut.N_Parts=N_Parts
        entcut.PARTS=PTR_NEW(parts,/no_copy)
        entcut.N_VERTICES = N_VERTICES
        wheatshp1->PutEntity, entcut

        cutattr.ATTRIBUTE_0 = y


        wheatshp1->setattributes,y,cutattr
        y=y+1
      endif
    endif

  endfor

  OBJ_DESTROY, oshp
  OBJ_DESTROY, wheatshp1
  ;  ENVI_REPORT_INIT, base=base, /finish

end

; FIXME
;�ü�����Ҫ���ηָ��Ӱ��
PRO Cal_subset,infile=infile, shapefile=shapefile, resultfile=resultfile
  compile_opt idl2

  CATCH, Error_status
  errorshow = 'Sorry to see the error'
  IF Error_status NE 0 THEN BEGIN
    tmp = DIALOG_MESSAGE(errorshow+STRING(13b)+$
      !ERROR_STATE.MSG,/error,title = '������ʾ!')
    return
  ENDIF

  shapeobj = OBJ_NEW('IDLffShape', shapefile)
  ENVI_OPEN_FILE,infile,r_fid = fid
  ENVI_FILE_QUERY, fid, ns = ns, nb = nb, nl = nl, dims = dims,BNAMES = BNAMES
  shapeobj->GETPROPERTY, N_Entities = nEntities
  ;
  ; shape_type =5--�����  8-- ��������
  ;BOUNDS �߽�ֵ
  ;
  roi_ids = LONARR(nEntities>1)
  FOR i=0, nEntities-1 DO BEGIN
    entitie = shapeobj->GETENTITY(i)
    ;����������ת�����������κβ���
    IF (entitie.SHAPE_TYPE EQ 5)  THEN BEGIN
      record = *(entitie.VERTICES)
      ;ת��Ϊ�ļ�����
      ENVI_CONVERT_FILE_COORDINATES,fid,xmap,ymap,record[0,*],record[1,*]
      ;����ROI
      roi_ids[i] = ENVI_CREATE_ROI(color=4,  $
        ns = ns ,  nl = nl)
      ENVI_DEFINE_ROI, roi_ids[i], /polygon, xpts=REFORM(xMap), ypts=REFORM(yMap)
      ;roi_ids[i] = roi_id
      ;��¼XY�����䣬�ü���
      ;��¼XY�����䣬�ü���
      IF (i EQ 0) THEN BEGIN
        xmin = ROUND(MIN(xMap,max = xMax))
        yMin = ROUND(MIN(yMap,max = yMax))
      ENDIF ELSE BEGIN
        xmin = xMin < ROUND(MIN(xMap))
        xMax = xMax > ROUND(MAX(xMap))
        yMin = yMin < ROUND(MIN(yMap))
        yMax = yMax > ROUND(MAX(yMap))
      ENDELSE
      
    ENDIF
    shapeobj->DESTROYENTITY, entitie
  ENDFOR
  OBJ_DESTROY, shapeobj
  
  ;
  if ((xMin ne !NULL) and (xmax ne !NULL) and (yMin ne !NULL) and (ymax ne !NULL) ) then begin
    xMin = xMin >0
    xmax = xMax < ns-1
    yMin = yMin >0
    ymax = yMax < nl-1
    
    out_dims = [-1,xMin,xMax,yMin,yMax]
    ;��ȡENVI�����ò���
    cfg = envi_get_configuration_values()
    tmppath = cfg.DEFAULT_TMP_DIRECTORY

    ;������Ĥ���ü�����
    ENVI_MASK_DOIT,$
      AND_OR =1, $
      OUT_NAME = tmppath+path_sep()+'void.mask', $
      ROI_IDS= roi_ids, $ ;ROI��ID
      ns = ns, nl = nl, $
      /inside, $ ;�����ڻ���
      r_fid = m_fid

    ENVI_MASK_APPLY_DOIT, FID = fid, POS = INDGEN(nb), DIMS = out_dims, $
      M_FID = m_fid, M_POS = [0], VALUE = 0, $
      out_name = resultfile
    ;out_bnames = BNAMES+"("+"subset by "+STRTRIM(FILE_BASENAME(shapefile),2)+")"
    ;��Ĥ�ļ�ID�Ƴ�
    ENVI_FILE_MNG, id =m_fid,/remove
    
  endif else begin
    ; shp �ļ�Ϊ�յ����
    
  endelse
END



;��������Ҫ���ηָ���ȡ��Ӱ������������������ȡ������Ҫ�Ĺ����ļ�
pro writerule,min_b1,max_b1,min_b2,max_b2,min_b3,max_b3,min_b4,max_b4,savepath
  min_b1=string(min_b1)+'.00000'
  max_b1=string(max_b1)+'.00000'
  min_b2=string(min_b2)+'.00000'
  max_b2=string(max_b2)+'.00000'
  min_b3=string(min_b3)+'.00000'
  max_b3=string(max_b3)+'.00000'
  min_b4=string(min_b4)+'.00000'
  max_b4=string(max_b4)+'.00000'
  oDocument = OBJ_NEW('IDLffXMLDOMDocument')
  oclasses = oDocument->createElement('classes')
  oclasses->SetAttribute,'name','All classes'
  oVoid = oDocument->APPENDCHILD(oclasses)
  oclass = oDocument->createElement('class')
  oclass->SetAttribute,'color','#0000FF'
  oclass->SetAttribute,'name','wheat'
  oclass->SetAttribute,'threshold','0.50'

  ;��һ����
  oVoid = oclasses->APPENDCHILD(oclass)
  orule = oDocument->createElement('rule')
  orule->SetAttribute,'weight','1.0'
  oattr = oDocument->createElement('attribute')
  oattr->SetAttribute,'algorithm','binary'
  oattr->SetAttribute,'band','0'
  oattr->SetAttribute,'name','Spectral_Mean'
  oattr->SetAttribute,'operation','between'
  oattr->SetAttribute,'tolerance','5'
  oattr->SetAttribute,'value',min_b1+','+max_b1
  oattr->SetAttribute,'weight','1.0'
  oVoid = orule->APPENDCHILD(oattr)
  oVoid = oclass->APPENDCHILD(orule)

  ;�ڶ�����
  oVoid = oclasses->APPENDCHILD(oclass)
  orule = oDocument->createElement('rule')
  orule->SetAttribute,'weight','1.0'
  oattr = oDocument->createElement('attribute')
  oattr->SetAttribute,'algorithm','binary'
  oattr->SetAttribute,'band','1'
  oattr->SetAttribute,'name','Spectral_Mean'
  oattr->SetAttribute,'operation','between'
  oattr->SetAttribute,'tolerance','5'
  oattr->SetAttribute,'value',min_b2+','+max_b2
  oattr->SetAttribute,'weight','1.0'
  oVoid = orule->APPENDCHILD(oattr)
  oVoid = oclass->APPENDCHILD(orule)

  ;��������
  oVoid = oclasses->APPENDCHILD(oclass)
  orule = oDocument->createElement('rule')
  orule->SetAttribute,'weight','1.0'
  oattr = oDocument->createElement('attribute')
  oattr->SetAttribute,'algorithm','binary'
  oattr->SetAttribute,'band','0'
  oattr->SetAttribute,'name','Spectral_Mean'
  oattr->SetAttribute,'operation','between'
  oattr->SetAttribute,'tolerance','5'
  oattr->SetAttribute,'value',min_b3+','+max_b3
  oattr->SetAttribute,'weight','1.0'
  oVoid = orule->APPENDCHILD(oattr)
  oVoid = oclass->APPENDCHILD(orule)

  ;���Ĳ���
  oVoid = oclasses->APPENDCHILD(oclass)
  orule = oDocument->createElement('rule')
  orule->SetAttribute,'weight','1.0'
  oattr = oDocument->createElement('attribute')
  oattr->SetAttribute,'algorithm','binary'
  oattr->SetAttribute,'band','0'
  oattr->SetAttribute,'name','Spectral_Mean'
  oattr->SetAttribute,'operation','between'
  oattr->SetAttribute,'tolerance','5'
  oattr->SetAttribute,'value',min_b4+','+max_b4
  oattr->SetAttribute,'weight','1.0'
  oVoid = orule->APPENDCHILD(oattr)
  oVoid = oclass->APPENDCHILD(orule)


  oVoid = oclasses->APPENDCHILD(oclass)
 
  oDocument->SAVE, FILENAME=savePath,/PRETTY_PRINT,ENCODING='utf-8'
  
end


;����Ҫ���ηָ���ȡ��Ӱ������������������ȡ
pro EXAMPLE_FX_RULEBASED_DOIT,file=file,rule_file=rule_file,vector_filename=vector_filename
  compile_opt IDL2
  ;
  ; Initialize ENVI and send all errors
  ; and warnings to the file batch.txt
  ;
  e = ENVI(/CURRENT)

  temp_dir = e.GetPreference('TEMPORARY_DIRECTORY')
  e.LOG_FILE = temp_dir+'batch.txt'

  ENVI_REPORT_INIT, ['ң��Ӱ��: '+File, '�����ļ�: '+rule_file,'���ݴ�����...'], $
    title="�������������ȡ", $
    base=base

  ;
  ; Open the input file
  ;
  file=file
  raster = e.OpenRaster(file)
  fid = ENVIRasterToFID(raster)
  ; Open rule file
  rule_file=rule_file
  ;
  ; Set output filenames
  ;
  report_filename = temp_dir+'report.txt'
  confidence_raster_filename = temp_dir+'confidence.dat'
  classification_raster_filename =temp_dir+'class.dat'
  segmentation_raster_filename = temp_dir+'segmentation.dat'
  vector_filename=vector_filename
  ;
  ; Set the keywords.
  dims = [-1L, 0, raster.ncolumns-1, 0, raster.nrows-1]
  pos = lindgen(raster.nbands); process all bands
  ;
  ; Perform rule-based classification
  ;
  envi_doit, 'envi_fx_rulebased_doit', $
    fid=fid, pos=pos, dims=dims, $
    r_fid=r_fid, merge_level=80.0, $
    scale_level=40.0, $
    rule_filename=rule_file, $
    segmentation_raster_filename=segmentation_raster_filename, $
    report_filename=report_filename, $
    confidence_raster_image=confidence_raster_filename, $
    classification_raster_filename=classification_raster_filename,$
    vector_filename=vector_filename
  ;
  ; get the returned file ID (R_FID)
  ;
  segmentation_raster = ENVIFIDToRaster(r_fid)
  ;segmentation_raster_URI = segmentation_raster.URI
  segmentation_raster.close

  ENVI_REPORT_INIT, base=base, /finish
end

;��������С�����ʸ���ļ�
pro wheat_shp,wheatshp_file=wheatshp_file,vector_filename=vector_filename

  compile_opt idl2
  e = ENVI(/current)
  CATCH, err
  if (err ne 0) then begin
    CATCH, /CANCEL
    if OBJ_VALID(e) then $
      e.ReportError, 'ERROR: ' + !ERROR_STATE.MSG
    MESSAGE, /RESET
    RETURN
  endif

  ENVI_REPORT_INIT, ['С��ʸ���ļ�: '+wheatshp_file, '���ηָ������ļ�: '+vector_filename,'���ݴ�����...'], $
    title="С�����ʸ���ļ�", $
    base=base

  wheatshp1=obj_new('IDLffshape',wheatshp_file,/update)
  wheatshp1->GETPROPERTY,n_entities=num_entities
  ;n_entis=num_entities-1

  entcut = {IDL_SHAPE_ENTITY}
  entcut.SHAPE_TYPE = 5
  cutattr = wheatshp1->GetAttributes(/ATTRIBUTE_STRUCTURE)


  ;��ȡshp�ļ�����Ϣ
  oshp=OBJ_NEW('IDLffShape',vector_filename, /update)
  oshp->GETPROPERTY,n_entities=n_ent



  iProj = ENVI_PROJ_CREATE(/geographic)
  ;�Զ���ȡprj�ļ���ȡͶӰ����ϵ
  potPos = STRPOS(vector_filename,'.',/reverse_search)  ;
  prjfile = STRMID(vector_filename,0,potPos[0])+'.prj'

  if FILE_TEST(prjfile) then begin
    OPENR, lun, prjFile, /GET_LUN
    strprj = ''
    READF, lun, strprj
    FREE_LUN, lun

    case STRMID(strprj, 0,6) of
      'GEOGCS': begin
        iProj = ENVI_PROJ_CREATE(PE_COORD_SYS_STR=strprj, $
          type = 1)
      end
      'PROJCS': begin
        iProj = ENVI_PROJ_CREATE(PE_COORD_SYS_STR=strprj, $
          type = 42)
      end
    endcase
  endif

  for i = 0, n_ent-1 do begin
    ;
    ent = oshp->GETENTITY(i, /ATTRIBUTES) ;��i����¼
    N_VERTICES=ent.N_VERTICES ;�������
    parts=*(ent.PARTS)
    verts=*(ent.VERTICES)
    N_Parts = N_ELEMENTS(Parts)
    attr=oshp->getattributes(i)

    if attr.attribute_1 eq 'wheat' then begin


      entcut.ISHAPE=num_entities
      entcut.BOUNDS = [min(verts[0,*]),min(verts[1,*]),0,0,max(verts[0,*]),max(verts[1,*]),0,0]
      pvertice=verts
      entcut.VERTICES=PTR_NEW(pvertice,/no_copy)
      entcut.N_Parts=N_Parts
      entcut.PARTS=PTR_NEW(parts,/no_copy)
      entcut.N_VERTICES = N_VERTICES
      wheatshp1->PutEntity, entcut
      cutattr.ATTRIBUTE_0 = num_entities
      wheatshp1->setattributes,num_entities,cutattr

      num_entities=num_entities+1
    endif
  endfor
  OBJ_DESTROY, oshp
  OBJ_DESTROY, wheatshp1
  ENVI_REPORT_INIT, base=base, /finish
end





















