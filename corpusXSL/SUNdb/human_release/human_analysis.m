% code of human performance analysis

% to compute the statistics in Section 3 of the following paper
% J. Xiao, J. Hays, K. A. Ehinger, A. Oliva, and A. Torralba
% SUN Database: Large-scale Scene Recognition from Abbey to Zoo
% Proceedings of 23rd IEEE Conference on Computer Vision and Pattern Recognition (CVPR2010)

%%

load TurkResult

%    1         2        3 id     4 id   5 text   6   7       8         9       10     11
% imgname  workerID  answer1  answer2  answer3  gt3 gt3#  correct1 correct2 correct3 answer3#

n=size(TurkResult,1);

for i=1:n
    if TurkResult{i,5}(1) ~= '/'
        try
            TurkResult{i,5}=['/' TurkResult{i,5}(1) '/' TurkResult{i,5}];
        catch err
            TurkResult{i,5}='/s/shower';
        end
    end
end

% level 3
for i=1:n
    h=findstr(TurkResult{i,1},'/sun_');
    TurkResult{i,6} = TurkResult{i,1}(1:h-1);
end
for i=1:n
    TurkResult{i,10}=strcmp(TurkResult{i,5},TurkResult{i,6});
end

%convert to number
load labelMat.mat
for i=1:n
    for j=1:size(labelMat,1)
        if strcmp(TurkResult{i,6},labelMat{j,1})
            TurkResult{i,7}=j;
            break;
        end
    end
end

for i=1:n
    if strcmp(TurkResult{i,5},'/i/initial')
        disp(['HIT ' num2str(i) ' ' TurkResult{i,1} ' has no label by workerID ' TurkResult{i,2}])
        TurkResult{i,11}=0;
    else
        for j=1:size(labelMat,1)
            if strcmp(TurkResult{i,5},labelMat{j,1})
                TurkResult{i,11}=j;
                break;
            end
        end
    end
end


% level 1
for i=1:n
    TurkResult{i,8} = labelMat{TurkResult{i,7},TurkResult{i,3}+2};
end

% level 2
for i=1:n
    offset = mod(TurkResult{i,4},10);
    level1 = (TurkResult{i,4}-offset)/10;
    switch level1
        case 1
            index =  5 + offset;
        case 2
            index = 11 + offset;
        case 3
            index = 15 + offset;
    end
    TurkResult{i,9} = labelMat{TurkResult{i,7},index};
end

val_L1 = sum(cell2mat(TurkResult(:, 8)));
val_L2 = sum(cell2mat(TurkResult(:, 9)));
val_L3 = sum(cell2mat(TurkResult(:,10)));

disp(['Overall Correctness Rate :']);
disp(['    Level 1 =' num2str(val_L1/n) ' ( ' num2str(val_L1) '/' num2str(n) ' )']);
disp(['    Level 2 =' num2str(val_L2/n) ' ( ' num2str(val_L2) '/' num2str(n) ' )']);
disp(['    Level 3 =' num2str(val_L3/n) ' ( ' num2str(val_L3) '/' num2str(n) ' )']);


[workerID foo Index]=unique(TurkResult(:,2));
workerCNT = zeros([size(workerID,1) 7]);

for i=1:n
    workerCNT(Index(i),1) = workerCNT(Index(i),1)+TurkResult{i,8};
    workerCNT(Index(i),2) = workerCNT(Index(i),2)+TurkResult{i,9};
    workerCNT(Index(i),3) = workerCNT(Index(i),3)+TurkResult{i,10};
    workerCNT(Index(i),4) = workerCNT(Index(i),4)+1;
end

for i=1:size(workerCNT,1)
    workerCNT(i,5) = workerCNT(i,1) / workerCNT(i,4);
    workerCNT(i,6) = workerCNT(i,2) / workerCNT(i,4);
    workerCNT(i,7) = workerCNT(i,3) / workerCNT(i,4);
end

hist(workerCNT(:,6:7),0:0.1:1)
ylabel('number of workers')
xlabel('correctness rate')
legend({'Level 2','Level 3'});
axis([-0.1 1.1 0 60])

for i=1:n
    truth_label(i) = TurkResult{i,7};
    mturk_label(i) = TurkResult{i,11};
end
good_index = find(mturk_label ~= 0);
truth_label = truth_label(good_index);
mturk_label = mturk_label(good_index);
C = confusionMatrix(truth_label, mturk_label);
save('all_workers_confusion.mat','C', 'labelMat')

labelMatMat = cell2mat(labelMat(1:397,3:end));

for j=1:19
    index =find(labelMatMat(:,j)==1);
    acc(j) = mean(diag(C(index,index)));
end
acc(20) = mean(diag(C));

catego{1} = 'indoor';
catego{2} = 'outdoor, natural';
catego{3} = 'outdoor, man-made';

catego{4} = 'shopping and dining';
catego{5} = 'workplace (office building, factory, lab, etc.)';
catego{6} = 'home or hotel';
catego{7} = 'transportation (vehicle interiors, stations, etc.)';
catego{8} = 'sports and leisure';
catego{9} = 'cultural (art, education, religion, etc.)';

catego{10} = 'water, ice, snow';
catego{11} = 'mountains, hills, desert, sky';
catego{12} = 'forest, field, jungle';	
catego{13} = 'man-made elements';	

catego{14} = 'transportation (roads, parking, bridges, boats, airports, etc.)';	
catego{15} = 'cultural or historical building/place';	
catego{16} = 'sports fields, parks, leisure spaces';	
catego{17} = 'industrial and construction';	
catego{18} = 'houses, cabins, gardens, and farms';
catego{19} = 'commercial buildings, shops, markets, cities, and towns';
catego{20} = 'all categories';
for j=1:20
    disp([num2str(acc(j)) ' ' catego{j}])
end

% for j=1:397
%     disp([sprintf('%2.3f',C(j,j)) '  ' labelMat{j,1}]);
% end


%% only good workers
disp('========================== only good workers ==========================');


deleteBadWorker = true;
if deleteBadWorker
    for i=1:size(workerCNT,1)
        GoodWorker(i) = 1;
        if (workerCNT(i,4)<100) || (workerCNT(i,5)<0.95)
            GoodWorker(i) = 0;
        end
%         if (~((workerCNT(i,4)<100) || (workerCNT(i,5)<0.95)))
%             GoodWorker(i) = 0;
%         end
    end
    TurkResult_old = TurkResult;
    clear TurkResultBad;
    clear TurkResult;
    n = 0;
    badn = 0;
    for i=1:size(TurkResult_old,1)
        for worker=1:size(workerCNT,1)
            if strcmp(workerID{worker},TurkResult_old{i,2})==1
                break;
            end
        end
        if GoodWorker(worker)
            n = n + 1;
            for k=1:size(TurkResult_old,2)
                TurkResult{n,k} = TurkResult_old{i,k};
            end
        else
            badn = badn + 1;
            for k=1:size(TurkResult_old,2)
                TurkResultBad{badn,k} = TurkResult_old{i,k};
            end            
        end
    end
end





n=size(TurkResult,1);


val_L1 = sum(cell2mat(TurkResult(:, 8)));
val_L2 = sum(cell2mat(TurkResult(:, 9)));
val_L3 = sum(cell2mat(TurkResult(:,10)));

disp(['Overall Correctness Rate :']);
disp(['    Level 1 =' num2str(val_L1/n) ' ( ' num2str(val_L1) '/' num2str(n) ' )']);
disp(['    Level 2 =' num2str(val_L2/n) ' ( ' num2str(val_L2) '/' num2str(n) ' )']);
disp(['    Level 3 =' num2str(val_L3/n) ' ( ' num2str(val_L3) '/' num2str(n) ' )']);

clear truth_label
clear mturk_label

for i=1:n
    truth_label(i) = TurkResult{i,7};
    mturk_label(i) = TurkResult{i,11};
end
good_index = find(mturk_label ~= 0);
truth_label = truth_label(good_index);
mturk_label = mturk_label(good_index);

C = confusionMatrix(truth_label, mturk_label);


save('good_workers_confusion.mat','C', 'labelMat')



CnonD = C - diag(diag(C));
imagesc(CnonD);
axis equal
cbar = autumn; 
cbar(1,:)=[0 0 0];
colormap(cbar);



for j=1:19
    index =find(labelMatMat(:,j)==1);
    acc(j) = mean(diag(C(index,index)));
end
acc(20) = mean(diag(C));

for j=1:20
    disp([num2str(acc(j)) ' ' catego{j}])
end

workerID_good  = workerID(find(GoodWorker==1));
workerCNT_good = workerCNT(find(GoodWorker==1),:);


acc_perworker_sorted = sort(workerCNT_good(:,end),'descend');
bar(acc_perworker_sorted);
axis([0 14 0 1])
xlabel('worker')
ylabel('accuracy')
