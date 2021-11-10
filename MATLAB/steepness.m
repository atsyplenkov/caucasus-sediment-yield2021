addpath(genpath('C:\Software\topotoolbox'))
rmpath(genpath('C:\Software\topotoolbox\.git'));

%% tutorial from 
% https://topotoolbox.wordpress.com/2017/08/22/steepness-derived-from-smoothed-river-profiles/comment-page-1/
DEM = GRIDobj("C:\Users\atsyp\YandexDisk\papers\2021\caucasus-sediment-yield2021\data\dem\aster_aoi_compressed.tif");
FD = FLOWobj(DEM,'preprocess','carve');
A = flowacc(FD);

% We then extract the stream network for a threshold upstream area of 1 sqkm.
S = STREAMobj(FD,'minarea',1e6,'unit','map');
%S = klargestconncomps(S,1);

% To hydrologically correct and smooth the river profile, we use the function crs:
zs = crs(S,DEM,'K',6,'tau',0.1);

k = ksn(S,zs,A,0.45);

%%
plotc(S,k);
imageschs(DEM,[],'colormap',[1 1 1],'colorbar',false,'ticklabels','nice');
hold on
plotc(S,k);
colormap(jet);
hx = colorbar;
hx.Label.String = 'k_{sn}';

%% SAVE
MS = STREAMobj2mapstruct(S,'seglength',1000,'attributes',{'ksn' k @mean});

shapewrite(MS, "C:\Users\atsyp\YandexDisk\papers\2021\caucasus-sediment-yield2021\data\spatial\temp\ksn_all.shp")
