function dictionary = readScenesDictionary
% save the third page of new_dictionary_complete.xls as a tab separated
% text.
fid = fopen('new_dictionary_complete.txt');
C = textscan(fid, '%s%s%s%s%s%s%s%s%s%s%s','Delimiter', '\t');
fclose(fid);

dictionary.folders = C{2};
dictionary.definition = C{3};


%{
	            %keyboard
            % Find definition (estas lineas se podrian reemplazar por una
            % llamada a wordnet).
            ndic = strmatch(Di(1).annotation.folder(3:end), dictionary.folders, 'exact');
            if ~isempty(ndic)
                definition = dictionary.definition{ndic};
            else
                definition = '  ';
            end
        }%