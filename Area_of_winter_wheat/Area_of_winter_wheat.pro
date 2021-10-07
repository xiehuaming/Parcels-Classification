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
     
      ;设定临界值
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
      ;即作为是否为纯地块的判断值又作为二次分割后面向对象特征提取模块中所需的fx值
      fx=fx
      Raster = pState.RASTER
      shpFile = pState.SHPFILE
      inFile=pState.INFILE
      
      ;该路径在pro wheat_shp1中作为第一次判断出的小麦地块数据，在pro wheat_shp中为最终解译的汇总结果
      wheatshp_file=pState.WHEATSHP_FILE
       WIDGET_CONTROL, ev.TOP, /DESTROY
       
      ;处理所需的各种路径     
      potPos = STRPOS(shpFile,'.',/reverse_search)
      prjfile = STRMID(shpFile,0,potPos[0])+'.prj'
      prjbasename=file_baseName(prjfile)
      testfile='c:\test\'+prjbasename
      wheatshp_path=file_dirName(wheatshp_file)
      tempprj = wheatshp_path+'\'+prjbasename
      wheatpotPos = STRPOS(wheatshp_file,'.',/reverse_search)
      wheatprjfile = STRMID(wheatshp_file,0,wheatpotPos[0])+'.prj'
      wheatprj=file_baseName(wheatprjfile)
      
      ;创建临时文件夹
      spawn,'md C:\test',/hide   
      spawn,['copy', prjfile,' c:\test\'],/hide
      spawn,['rename',testfile,'test1.prj'],/hide
      spawn,['copy', prjfile,' c:\test\'],/hide
      spawn,['rename',testfile,' cutshape.prj'],/hide
      spawn,['copy', prjfile,wheatshp_path],/hide
      spawn,['rename',tempprj,wheatprj],/hide
      
      ;属性表中有遥感影像光谱信息的地块数据
      shapefile='c:\test\test1.shp'
      
      ;rule文件存储位置
      savepath='c:/test/wheat.rul'
      
      ;用于二次分割所需的地块数据
      cutshapefile='c:\test\cutshape.shp'          

      ;将遥感影像的光谱信息添加到矢量文件的属性表中
      Areaofwinterwheat, Raster, shpFile,inFile,shapefile,$
  min_b1,max_b1,$
  min_b2,max_b2,$
  min_b3,max_b3,$
  min_b4,max_b4
          
      ;生成需要二次分割所需的矢量文件
      cutshape,shapefile=shapefile,cutshapefile=cutshapefile,fx=fx
      ;提取出纯地块中的小麦部分
      wheat_shp1,shapefile=shapefile,wheatshp_file=wheatshp_file,fx=fx
  
      resultfile='c:\test\test.img'
      ;裁剪出 对需要二次分割提取的影像进行面向对象特征提取’所需的遥感影像
      cal_subset,infile=infile, shapefile=cutshapefile, resultfile=resultfile


      ;创建‘需要二次分割提取的影像进行面向对象特征提取’中所需要的规则文件
      writerule,min_b1,max_b1,min_b2,max_b2,min_b3,max_b3,min_b4,max_b4,savepath

      rule_file='c:\test\wheat.rul'
      vector_filename='c:\test\vector.shp'
      ;对需要二次分割提取的影像进行面向对象特征提取
      EXAMPLE_FX_RULEBASED_DOIT,file=resultfile,rule_file=rule_file,vector_filename=vector_filename

      ;将混合地块中的小麦部分与纯地块中的小麦部分进行合并生成最终的小麦地块文件
      wheat_shp,wheatshp_file=wheatshp_file,vector_filename=vector_filename

      ;从窗口中移除影像数据
      Raster.Close

      ;删除临时文件夹(如需查看过程数据可保留该文件夹)
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
  ;选择遥感影像
  UI = e.UI
  Raster = UI.SelectInputData(/RASTER, Bands=Bands, Sub_RECT=Sub_RECT, $
    title = '选择遥感影像', /DISABLE_NO_DATA)
  if Raster eq !NULL then RETURN
  inFile = Raster.URI
  Raster = ENVISUBSETRASTER(Raster, Bands=Bands, SUB_RECT=Sub_RECT)

  ;显示遥感影像
  view=e.getview()
  layer1=view.createlayer(Raster,/cir)


  ;选择地块数据（shp文件）
  Vector = UI.SelectInputData(/VECTOR, title='选择地块数据',$
    /DISABLE_NO_DATA)
  if Vector eq !NULL then RETURN
  shpFile = Vector.URI

  ;显示地块数据
  oshp=e.OpenVector(shpfile)
  layer2=view.createlayer(oshp,/cir)


  ENVI_CENTER, xoff, yoff
  tlb = WIDGET_BASE(TITLE='小麦地块', /COLUMN, $
    XOFFSET=xoff, YOFFSET=yoff, TLB_FRAME_ATTR=1)

  ;文件路径显示
  filebase=WIDGET_BASE(tlb, /column, /FRAME)
  label1=widget_label(filebase,value='遥感影像：'+inFile)
  label2=widget_label(filebase,value='地块数据：'+shpFile)
  
  ;结果输出路径
  wheat_file=dialog_pickfile(title='请选择结果输出路径',filter='*.shp')
  wheatshp_file=wheat_file+'.shp'
  ;编写规则（填写各个波段的最大值与最小值）
  rulebase=WIDGET_BASE(tlb, /column, /FRAME)
  ;第一波段
  rulebase1=WIDGET_BASE(rulebase, /ROW, /FRAME)
  rulelabel1=widget_label(rulebase1,value='第一波段：')
  min_b1 = CW_FIELD(rulebase1,TITLE = "最小值：",/FRAME,VALUE=260, /INTEGER)
  max_b1 = CW_FIELD(rulebase1,TITLE = "最大值：",/FRAME,VALUE=290, /INTEGER)
  ;第二波段
  rulebase2=WIDGET_BASE(rulebase, /ROW, /FRAME)
  rulelabel2=widget_label(rulebase2,value='第二波段：')
  min_b2 = CW_FIELD(rulebase2,TITLE = "最小值：",/FRAME,VALUE=227, /INTEGER)
  max_b2 = CW_FIELD(rulebase2,TITLE = "最大值：",/FRAME,VALUE=279, /INTEGER)
  ;第三波段
  rulebase3=WIDGET_BASE(rulebase, /ROW, /FRAME)
  rulelabel3=widget_label(rulebase3,value='第三波段：')
  min_b3 = CW_FIELD(rulebase3,TITLE = "最小值：",/FRAME,VALUE=190, /INTEGER)
  max_b3 = CW_FIELD(rulebase3,TITLE = "最大值：",/FRAME,VALUE=251, /INTEGER)
  ;第四波段
  rulebase4=WIDGET_BASE(rulebase, /ROW, /FRAME)
  rulelabel4=widget_label(rulebase4,value='第四波段：')
  min_b4 = CW_FIELD(rulebase4,TITLE = "最小值：",/FRAME,VALUE=282, /INTEGER)
  max_b4 = CW_FIELD(rulebase4,TITLE = "最大值：",/FRAME,VALUE=397, /INTEGER)
  ;离散度
  rulebasefx=WIDGET_BASE(rulebase, /ROW, /FRAME)
  fx = CW_FIELD(rulebasefx,TITLE = "离散度：",/FRAME,VALUE=57, /INTEGER)


  ;下一步按钮
  okBase = WIDGET_BASE(tlb, /ROW, /FRAME)
  okBtn = WIDGET_BUTTON(okBase, value='下一步', uname='next', XSIZE=60)
  cancelBtn = WIDGET_BUTTON(okBase, value='取消',uname='Cancel',XSIZE=80)

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




;将遥感影像的光谱信息添加到矢量文件的属性表中
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

  ;创建shp
  shapefile=shapefile
  newshp=obj_new('IDLffshape',shapefile,Entity_type=5,/update)
  ;定义属性表结构
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

  ;读取shp文件的信息
  oshp=OBJ_NEW('IDLffShape',shpFile)
  oshp->GETPROPERTY,n_entities=n_ent

  iProj = ENVI_PROJ_CREATE(/geographic)
  ;自动读取prj文件获取投影坐标系
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


  ENVI_REPORT_INIT, ['遥感影像: '+inFile, '地块数据: '+shpFile], $
    title="小麦面积", $
    base=base

  ENVI_REPORT_INC, base, n_ent

  ;循环中，使用每条shp记录，创建roi
  ;然后使用ROI进行掩膜统计
  for i = 0, n_ent-1 do begin
    ;
    ent = oshp->GETENTITY(i, /ATTRIBUTES) ;第i条记录
    N_VERTICES=ent.N_VERTICES ;顶点个数
    parts=*(ent.PARTS)
    verts=*(ent.VERTICES)

    ; 将顶点坐标转换为输入文件的地理坐标
    ENVI_CONVERT_PROJECTION_COORDINATES,  $
      verts[0,*], verts[1,*], iProj,    $
      oXmap, oYmap, oProj
    ; 转换为文件坐标
    ENVI_CONVERT_FILE_COORDINATES,fid,    $
      xFile,yFile,oXmap,oYmap

    xFile = xFile > 0 < ns
    yFile = yFile > 0 < nl

    sub_Rect = LONG64([MIN(xFile),MIN(yFile),MAX(xFile),MAX(yFile)])

    subRaster = ENVISUBSETRASTER(Raster, SUB_RECT=sub_Rect)
    subFid = ENVIRASTERTOFID(subRaster)
    ENVI_FILE_QUERY, subFid, ns=subNS, nl=subNL
    ; 转换为文件坐标
    ENVI_CONVERT_FILE_COORDINATES, subFid,    $
      xFile,yFile,oXmap,oYmap

    xFile = LONG64(xFile)
    yFile = LONG64(yFile)

    ;创建ROI
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

      ;如果有的ROI像元数为0，则不保存
      ENVI_GET_ROI_INFORMATION, roi_id, NPTS=npts
      if npts eq 0 then continue

      roi_ids = [roi_ids, roi_id]
    endfor

    ENVI_REPORT_STAT,base, i, n_ent

    ; 判断ROI个数 20210907
    if roi_ids eq !NULL then continue
    ; 判断ROI个数 end 
    
    ;    name = Attrs[attrIdx, i]
    roiFile = e.GetTemporaryFilename('roi')
    ENVI_SAVE_ROIS, roiFile, roi_ids

    ;调用ENVI接口进行掩膜、统计
    roi = e.OpenROI(roiFile)
    rasterWithMask = ENVIROIMASKRASTER(SubRaster, roi)
    ;波段不为1时，单独统计COUNTS
    stats = ENVIRasterStatistics(ENVISUBSETRASTER(rasterWithMask,bands=[0]), $
      /HISTOGRAMS, HISTOGRAM_NBINS=1)
    Hist = stats.HISTOGRAMS
    Hist = Hist[0]
    COUNTS = Hist.COUNTS

    ;统计min、max、mean等值
    stats = ENVIRasterStatistics(rasterWithMask)
    NDVImean=(stats.MEAN[3]-stats.MEAN[2])/(stats.MEAN[3]+stats.MEAN[2])

    ;复制shp
    entNew.ISHAPE=x
    entNew.BOUNDS = [min(verts[0,*]),min(verts[1,*]),0,0,max(verts[0,*]),max(verts[1,*]),0,0]
    pvertice=verts
    entnew.VERTICES=PTR_NEW(pvertice,/no_copy)
    entNew.N_Parts=N_Parts
    entnew.PARTS=PTR_NEW(parts,/no_copy)
    entNew.N_VERTICES = N_VERTICES
    newshp->PutEntity, entNew

    attr.ATTRIBUTE_0 = x
    ;判断是否是小麦
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



;提取出需要裁剪影像所需的shp文件
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

  ;设定临界值
  y=0
  fx=fx
  ;创建shp
  cutshapefile=cutshapefile
  cutshp=obj_new('IDLffshape',cutshapefile,Entity_type=5,/update)
  ;定义属性表结构
  cutshp->AddAttribute,'ID',3,8,PRECISION=0

  entcut = {IDL_SHAPE_ENTITY}
  entcut.SHAPE_TYPE = 5
  cutattr = cutshp->GetAttributes(/ATTRIBUTE_STRUCTURE)
  cutshp->AddAttribute,'ID',3,8,PRECISION=0

  ;读取shp文件的信息
  oshp=OBJ_NEW('IDLffShape',shapefile, /update)
  oshp->GETPROPERTY,n_entities=n_ent

  iProj = ENVI_PROJ_CREATE(/geographic)
  ;自动读取prj文件获取投影坐标系
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
    ent = oshp->GETENTITY(i, /ATTRIBUTES) ;第i条记录
    N_VERTICES=ent.N_VERTICES ;顶点个数
    parts=*(ent.PARTS)
    verts=*(ent.VERTICES)
    N_Parts = N_ELEMENTS(Parts)
    attr=oshp->getattributes(i)

    if attr.attribute_6 ge fx then begin
      ;复制shp
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

;提取出初次获得的小麦地区shp文件
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

  ;设定临界值
  y=0
  fx=fx
  ;创建shp
  wheatshp_file=wheatshp_file
  wheatshp1=obj_new('IDLffshape',wheatshp_file,Entity_type=5,/update)
  ;定义属性表结构
  wheatshp1->AddAttribute,'ID',3,8,PRECISION=0

  entcut = {IDL_SHAPE_ENTITY}
  entcut.SHAPE_TYPE = 5
  cutattr = wheatshp1->GetAttributes(/ATTRIBUTE_STRUCTURE)
  wheatshp1->AddAttribute,'ID',3,8,PRECISION=0

  ;读取shp文件的信息
  oshp=OBJ_NEW('IDLffShape',shapefile, /update)
  oshp->GETPROPERTY,n_entities=n_ent

  iProj = ENVI_PROJ_CREATE(/geographic)
  ;自动读取prj文件获取投影坐标系
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
    ent = oshp->GETENTITY(i, /ATTRIBUTES) ;第i条记录
    N_VERTICES=ent.N_VERTICES ;顶点个数
    parts=*(ent.PARTS)
    verts=*(ent.VERTICES)
    N_Parts = N_ELEMENTS(Parts)
    attr=oshp->getattributes(i)
    ;离散度（第一波段标准差）判断是否为混合地块
    if attr.attribute_6 lt fx then begin
      if attr.attribute_1 eq 'wheat' then begin
        ;复制shp
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
;裁剪出需要二次分割的影像
PRO Cal_subset,infile=infile, shapefile=shapefile, resultfile=resultfile
  compile_opt idl2

  CATCH, Error_status
  errorshow = 'Sorry to see the error'
  IF Error_status NE 0 THEN BEGIN
    tmp = DIALOG_MESSAGE(errorshow+STRING(13b)+$
      !ERROR_STATE.MSG,/error,title = '错误提示!')
    return
  ENDIF

  shapeobj = OBJ_NEW('IDLffShape', shapefile)
  ENVI_OPEN_FILE,infile,r_fid = fid
  ENVI_FILE_QUERY, fid, ns = ns, nb = nb, nl = nl, dims = dims,BNAMES = BNAMES
  shapeobj->GETPROPERTY, N_Entities = nEntities
  ;
  ; shape_type =5--多边形  8-- 多个多边形
  ;BOUNDS 边界值
  ;
  roi_ids = LONARR(nEntities>1)
  FOR i=0, nEntities-1 DO BEGIN
    entitie = shapeobj->GETENTITY(i)
    ;多边形则进行转换，否则不做任何操作
    IF (entitie.SHAPE_TYPE EQ 5)  THEN BEGIN
      record = *(entitie.VERTICES)
      ;转换为文件坐标
      ENVI_CONVERT_FILE_COORDINATES,fid,xmap,ymap,record[0,*],record[1,*]
      ;创建ROI
      roi_ids[i] = ENVI_CREATE_ROI(color=4,  $
        ns = ns ,  nl = nl)
      ENVI_DEFINE_ROI, roi_ids[i], /polygon, xpts=REFORM(xMap), ypts=REFORM(yMap)
      ;roi_ids[i] = roi_id
      ;记录XY的区间，裁剪用
      ;记录XY的区间，裁剪用
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
    ;获取ENVI的配置参数
    cfg = envi_get_configuration_values()
    tmppath = cfg.DEFAULT_TMP_DIRECTORY

    ;创建掩膜，裁剪后掩
    ENVI_MASK_DOIT,$
      AND_OR =1, $
      OUT_NAME = tmppath+path_sep()+'void.mask', $
      ROI_IDS= roi_ids, $ ;ROI的ID
      ns = ns, nl = nl, $
      /inside, $ ;区域内或外
      r_fid = m_fid

    ENVI_MASK_APPLY_DOIT, FID = fid, POS = INDGEN(nb), DIMS = out_dims, $
      M_FID = m_fid, M_POS = [0], VALUE = 0, $
      out_name = resultfile
    ;out_bnames = BNAMES+"("+"subset by "+STRTRIM(FILE_BASENAME(shapefile),2)+")"
    ;掩膜文件ID移除
    ENVI_FILE_MNG, id =m_fid,/remove
    
  endif else begin
    ; shp 文件为空的情况
    
  endelse
END



;创建对需要二次分割提取的影像进行面向对象特征提取中所需要的规则文件
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

  ;第一波段
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

  ;第二波段
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

  ;第三波段
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

  ;第四波段
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


;对需要二次分割提取的影像进行面向对象特征提取
pro EXAMPLE_FX_RULEBASED_DOIT,file=file,rule_file=rule_file,vector_filename=vector_filename
  compile_opt IDL2
  ;
  ; Initialize ENVI and send all errors
  ; and warnings to the file batch.txt
  ;
  e = ENVI(/CURRENT)

  temp_dir = e.GetPreference('TEMPORARY_DIRECTORY')
  e.LOG_FILE = temp_dir+'batch.txt'

  ENVI_REPORT_INIT, ['遥感影像: '+File, '规则文件: '+rule_file,'数据处理中...'], $
    title="面向对象特征提取", $
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

;生成最终小麦面积矢量文件
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

  ENVI_REPORT_INIT, ['小麦矢量文件: '+wheatshp_file, '二次分割生成文件: '+vector_filename,'数据处理中...'], $
    title="小麦面积矢量文件", $
    base=base

  wheatshp1=obj_new('IDLffshape',wheatshp_file,/update)
  wheatshp1->GETPROPERTY,n_entities=num_entities
  ;n_entis=num_entities-1

  entcut = {IDL_SHAPE_ENTITY}
  entcut.SHAPE_TYPE = 5
  cutattr = wheatshp1->GetAttributes(/ATTRIBUTE_STRUCTURE)


  ;读取shp文件的信息
  oshp=OBJ_NEW('IDLffShape',vector_filename, /update)
  oshp->GETPROPERTY,n_entities=n_ent



  iProj = ENVI_PROJ_CREATE(/geographic)
  ;自动读取prj文件获取投影坐标系
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
    ent = oshp->GETENTITY(i, /ATTRIBUTES) ;第i条记录
    N_VERTICES=ent.N_VERTICES ;顶点个数
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





















